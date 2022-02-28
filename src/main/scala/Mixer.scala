import chisel3._
import chisel3.util._

import chisel3.experimental.dataview._
import chisel3.experimental.conversions._

import types._
import ram._
import models.RamInitUnit
import internal.firrtl.Width
import os.truncate

class MixerParameter(n : Int = 4) {
  val nFeatures = n

  val WeightWidth = 17.W
  val XWidth = 12.W
  val lossWidth = 18.W

  val WeightInitVal = (1 << (WeightWidth.get + 1)) / n
}

class BitProbsCtxBundle(n : Int) extends Bundle {
  val bit = UInt(1.W)
  val ctx = UInt(8.W)
  val probs = Vec(n, UInt(12.W))
  val last = Bool()
}

object XsBundle {
  def apply(n : Int) = {
    Vec(n, SInt(12.W))
  }
}

object WeightsBundle {
  def apply(n : Int) = {
    Vec(n, SInt(17.W))
  }
}

import  models.RamInitUnit

class DPRam[T <: Data](t : T, addr : Width) extends Module {
  val io = IO(new Bundle {
    val wea = Input(Bool())
    val addra = Input(UInt(addr))
    val dina = Input(t)

    val enb = Input(Bool())
    val addrb = Input(UInt(addr))
    val dob = Output(t)
  })

  val mem = SyncReadMem(1 << addr.get, t)

  io.dob := mem.read(io.addrb, io.enb)

  when(io.wea) {
    mem.write(io.addra, io.dina)
  }
}

class MACC(AWidth : Width, BWidth : Width, CWidth : Width) extends Module {
  val latency = 4

  val io = IO(new Bundle {
    val ce = Input(Bool())

    val a = Input(SInt(AWidth))
    val b = Input(SInt(BWidth))
    val reload = Input(Bool())

    val acc = Output(SInt(CWidth))
  })

  val a = RegEnable(io.a, io.ce)
  val b = RegEnable(io.b, io.ce)
  val reload = ShiftRegister(io.reload, 2 , true.B, io.ce)

  val mul = RegEnable(a * b, io.ce)
  val lastResult = Wire(SInt(CWidth))
  val acc = RegEnable(mul + lastResult, io.ce)

  lastResult := Mux(reload, 0.S, acc)

  io.acc := RegEnable(acc, io.ce)
}

// c - a * b
class MS(AWidth : Width, BWidth : Width, CWidth : Width) extends Module {
  val latency = 3
  val io = IO(new Bundle {
    val ce = Input(Bool())

    val a = Input(SInt(AWidth))
    val b = Input(SInt(BWidth))
    val c = Input(SInt(CWidth))

    val out = Output(SInt(CWidth))
  })

  val a = RegEnable(io.a, io.ce)
  val b = RegEnable(io.b, io.ce)
  val c = ShiftRegister(io.c, 2, io.ce)

  val mul = RegEnable(a * b, io.ce)
  val out = RegEnable((c << 16) + mul, io.ce) >> 16

  io.out := out
}

class PredictPE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val W = Flipped(DecoupledIO(WeightsBundle(p.nFeatures)))
    val X = Flipped(DecoupledIO(XsBundle(p.nFeatures)))
    val ctrl = Flipped(DecoupledIO(UInt(1.W)))

    val P = ValidIO(new BitProbBundle())

    val updateStrm = ValidIO(new Bundle {
      val w = SInt(p.WeightWidth)
      val x = SInt(p.XWidth)
    })
  })

  // ctrl signals
  val xLoad = Wire(Bool())
  val wLoad = Wire(Bool())
  val bitLoad = Wire(Bool())
  val wxSel = Wire(UInt(log2Ceil(p.nFeatures + 1).W))

  val maccReload = Wire(Bool())
  val maccCE = Wire(Bool())

  val pOut = Wire(Bool())

  // data path
  // X * W
  val X = RegEnable(io.X.bits, xLoad)
  val W = RegEnable(io.W.bits, wLoad)
  val bit0 = RegEnable(io.ctrl.bits, bitLoad)

  val x = X(wxSel)
  val w = W(wxSel)

  val macc = Module(new MACC(p.XWidth, p.WeightWidth, 32.W))
  macc.io.a := x
  macc.io.b := w
  macc.io.ce := maccCE
  macc.io.reload := maccReload

  val bit = ShiftRegister(bit0, macc.latency, maccCE)
  val prob = Squash(macc.io.acc >> 16)

  // Ctrl singls gen
  val cntCE = Wire(Bool())
  val (cnt, cntWrp) = Counter(0 until p.nFeatures, cntCE)

  val sIdle :: sWorking :: Nil = Enum(2)
  val stateNxt = Wire(UInt())
  val state = RegNext(stateNxt)

  val takeInput = io.X.fire && io.W.fire && io.ctrl.fire
  stateNxt := state
  switch(state) {
    is(sIdle) {
      when(takeInput) {
        stateNxt := sWorking
      }
    }

    is(sWorking) {
      when(cntWrp && ~takeInput) {
        stateNxt := sIdle
      }
    }
  }

  val inIdle = state === sIdle
  val inWorking = state === sWorking
  cntCE := inWorking

  xLoad := (inIdle && takeInput) || (inWorking && cntWrp)
  wLoad := xLoad
  bitLoad := wLoad
  wxSel := cnt

  maccReload := cnt === 0.U
  maccCE := true.B

  pOut := ShiftRegister(cntWrp, macc.latency)

  // outputs
  io.X.ready := inIdle || (inWorking && cntWrp)
  io.W.ready := io.X.ready
  io.ctrl.ready := io.X.ready

  io.P.bits.bit := bit
  io.P.bits.prob := prob
  io.P.bits.last := false.B //DontCare
  io.P.valid := pOut

  io.updateStrm.bits.x := ShiftRegister(x, p.nFeatures + macc.latency)
  io.updateStrm.bits.w := ShiftRegister(w, p.nFeatures + macc.latency)
  io.updateStrm.valid := ShiftRegister(inWorking, p.nFeatures + macc.latency, false.B, true.B)
}


class LossCalPE()(implicit p : MixerParameter) extends Module {
  val latency = 1

  val io = IO(new Bundle {
    val P = Input(ValidIO(new BitProbBundle()))

    val loss = Output(SInt(p.lossWidth))

    val updateStrmCAS = Input(ValidIO(new Bundle {
      val w = SInt(p.WeightWidth)
      val x = SInt(p.XWidth)
    }))

    val updateStrm = Output(ValidIO(new Bundle {
      val w = SInt(p.WeightWidth)
      val x = SInt(p.XWidth)
    }))
  })
  // ctrl signals
  val lossLoad = Wire(Bool())
  val probLoad = Wire(Bool())
  
  // data path
  val prob = RegEnable(io.P.bits.prob, probLoad)
  val bit = RegEnable(io.P.bits.bit, probLoad)
  val probExpect = 0.U(p.lossWidth) | Cat(bit, 0.U(12.W))

  val lr = 10.U(6.W)
  val lossCal = (probExpect - prob).asSInt * lr  // zero ext first?
  val loss = lossCal

  // ctrl signals gne
  lossLoad := io.P.valid
  probLoad := io.P.valid

  // output
  io.loss := loss

  io.updateStrm := Pipe(io.updateStrmCAS)
}

class UpdatePE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val loss = Input(SInt(p.lossWidth))

    val updateStrm = Flipped(ValidIO(new Bundle {
      val w = SInt(p.WeightWidth)
      val x = SInt(p.XWidth)
    }))

    val wStrm = ValidIO(SInt(p.WeightWidth))
  })
  // ctrl signals

  // data path
  val ms = Module(new MS(p.XWidth, p.lossWidth, p.WeightWidth))
  ms.io.ce := true.B
  ms.io.a := io.updateStrm.bits.x
  ms.io.b := io.loss
  ms.io.c := io.updateStrm.bits.w

  // ctrl signals gen

  // io
  io.wStrm.bits := ms.io.out
  io.wStrm.valid := ShiftRegister(io.updateStrm.valid, ms.latency, false.B, true.B)
}

class PredictUpdateEngine()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val W = Flipped(DecoupledIO(WeightsBundle(p.nFeatures)))
    val X = Flipped(DecoupledIO(XsBundle(p.nFeatures)))
    val ctrl = Flipped(DecoupledIO(UInt(1.W)))

    val P = ValidIO(new BitProbBundle())
    val Wu = ValidIO(SInt(p.WeightWidth))
  })

  val predictPE = Module(new PredictPE)
  val lossPE = Module(new LossCalPE)
  val updatePE = Module(new UpdatePE)

  predictPE.io.W <> io.W
  predictPE.io.X <> io.X
  predictPE.io.ctrl <> io.ctrl
  
  lossPE.io.P <> predictPE.io.P
  io.P <> predictPE.io.P
  lossPE.io.updateStrmCAS <> predictPE.io.updateStrm

  updatePE.io.loss := lossPE.io.loss
  updatePE.io.updateStrm <> lossPE.io.updateStrm

  io.Wu <> updatePE.io.wStrm
}

class MixerPE(n : Int, ForceFirstOutput : Boolean) extends Module {
  val WeightInitVal = (1 << 16) / n
  
  val WeightInitVec = VecInit(Seq.fill(n){WeightInitVal.S(17.W)})
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new BitProbsCtxBundle(n)))
    val out = ValidIO(new BitProbBundle)

    val status = new StatusBundle
  })

  val fire = ShiftRegisters(io.in.fire, 3)
  val X = ShiftRegisters(VecInit(io.in.bits.probs.map(Stretch(_))), 2)
  val ctx = ShiftRegisters(io.in.bits.ctx, 3)
  val bit = ShiftRegisters(io.in.bits.bit, 2)
  val last = RegNext(io.in.bits.last)

  val mem = Module(new DPRam(chiselTypeOf(WeightInitVec), log2Up(80).W)).io
  mem.enb := io.in.fire
  mem.addrb := io.in.bits.ctx

  val Wr = mem.dob

  val ramInit = Module(new RamInitUnit(log2Up(80))).io
  ramInit.in.bits := last(0)
  ramInit.in.valid := fire(0)

  val W0sel = Wire(Bool())
  val W0 = Wire(chiselTypeOf(Wr))
  val W1 = RegNext(W0)

  val mul = (X(0) zip W0).map {case (a, b) => a * b}
  val p = Squash(TreeReduce[SInt](mul, _ + _) >> 16)
  val p_d = RegNext(p)
  val p_exp = Cat(bit(1), 0.U(11.W))

  val lr = 55.U(6.W)
  val loss = (p_exp - p_d).asSInt * lr

  val Wu = RegNext(VecInit((0 until n).map(i => (W1(i) - X(1)(i) * loss).asTypeOf(SInt(17.W)))))

  W0 := Mux(W0sel, Wu, Wr)
  mem.wea := fire(2) || ramInit.wen
  mem.addra := Mux1H(Seq(
                      (fire(2), ctx(2)),
                      (ramInit.wen, ramInit.waddr),
                      ))
  mem.dina := Mux1H(Seq(
                      (fire(2), Wu),
                      (ramInit.wen, WeightInitVec),
                      ))

  val harzard = io.in.bits.ctx === ctx(0) && fire(0)
  val harzard_d = RegNext(harzard, false.B)
  val harzard_dd = RegNext(harzard_d, false.B)

  W0sel := harzard_dd

  io.in.ready := ~harzard

  io.out.valid := fire(0)
  io.out.bits.bit := bit(0)
  io.out.bits.last := last
  if(ForceFirstOutput) {
    val first = RegInit(true.B)
    when(~io.status.initDone) {
      first := true.B
    }.elsewhen(io.out.valid) {
      first := false.B
    }

    io.out.bits.prob := Mux(first, 2048.U, p)
  } else {
    io.out.bits.prob := p
  }

  io.status := ramInit.status
}

class Mixer(n : Int) extends Module {
  val nProb = n
  
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new BitProbsCtxBundle(n))))
    val out = Vec(8, ValidIO(new BitProbBundle))

    val status = new StatusBundle
  })

  val PEs = (0 until 8).map(i => Module(new MixerPE(n, i == 0)).io)
  for(i <- 0 until 8) {
    PEs(i).in <> io.in(i)
    PEs(i).out <> io.out(i)
  }
  io.status.initDone := TreeReduce[Bool](PEs.map(_.status.initDone), _ & _)
}