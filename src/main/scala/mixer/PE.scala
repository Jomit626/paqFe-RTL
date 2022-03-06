package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._

class PredictPE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new PredictUpdateEngineXCtrlBundle()))
    val W = Flipped(DecoupledIO(WeightsBundle()))

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
  val X = RegEnable(io.in.bits.X, xLoad)
  val W = RegEnable(io.W.bits, wLoad)
  val bit0 = RegEnable(io.in.bits.bit, bitLoad)

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

  val takeInput = io.in.fire && io.W.fire
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
  io.in.ready := inIdle || (inWorking && cntWrp)
  io.W.ready := io.in.ready

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
