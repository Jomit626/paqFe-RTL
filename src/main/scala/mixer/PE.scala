package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.util._

class VecDotPackBundle(implicit p : MixerParameter) extends Bundle {
  val x = Vec(p.nFeatures, SInt(p.XWidth))
  val w = Vec(p.nFeatures, SInt(p.WeightWidth))
  val last = Bool()
  val bit = UInt(1.W)
}

class PredictPE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VecDotPackBundle()))

    val P = ValidIO(new BitProbBundle())
    val x = ValidIO(XBitBundle())

    val updateStrm = DecoupledIO(new WeightUpdateBundle())
  })

  // ctrl signals
  val load = Wire(Bool())

  val wxSel = Wire(UInt(log2Ceil(p.VecDotII + 1).W))

  val macReload = Wire(Bool())
  val macCE = Wire(Bool())

  val output = Wire(Bool())

  // data path
  val bit0 = RegEnable(io.in.bits.bit, load)
  val last0 = RegEnable(io.in.bits.last, load)
  // Registers to store input W and X
  val WReg = Reg(Vec(p.nFeatures, SInt(p.WeightWidth)))
  val XReg = Reg(Vec(p.nFeatures, SInt(p.XWidth)))

  when(load) {
    for(i <- 0 until p.nFeatures) {
      XReg(i) := io.in.bits.x(i)
    }
    for(i <- 0 until p.nFeatures) {
      WReg(i) := io.in.bits.w(i)
    }
  }
  // Split W and X to MAC unit
  
  val W = Seq.fill(p.VecDotMACNum) {Wire(Vec(p.VecDotII, SInt(p.WeightWidth)))}
  val X = Seq.fill(p.VecDotMACNum) {Wire(Vec(p.VecDotII, SInt(p.XWidth)))}

  for(j <- 0 until p.VecDotII) {
    for(i <- 0 until p.VecDotMACNum) {
      if(j * p.VecDotMACNum + i < p.nFeatures) {
        W(i)(j) := WReg(j * p.VecDotMACNum + i)
        X(i)(j) := XReg(j * p.VecDotMACNum + i)
      } else {
        W(i)(j) := 0.S
        X(i)(j) := 0.S
      }
    }
  }

  // MAC Unit
  val macs = Seq.fill(p.VecDotMACNum) {Module(new MACC(p.WeightWidth, p.XWidth, 32.W))}
  macs.zipWithIndex.foreach {case (m, i) =>
    m.io.a := W(i)(wxSel)
    m.io.b := X(i)(wxSel)
    m.io.reload := macReload
    m.io.ce := macCE
  }
  val SquashLatency = 3 // TODO: retimming sum

  val dot = TreeReduce[SInt](macs.map(_.io.acc), (a, b) => a + b) >> 16
  val dotGt2047 = dot > 2047.S
  val dotLtn2048 = dot < -2048.S

  val probStrech = Wire(SInt(p.XWidth))
  probStrech := Mux(dotGt2047, 2047.S, Mux(dotLtn2048, -2048.S, dot))
  
  val prob = ShiftRegister(Squash(probStrech), SquashLatency)
  val x = ShiftRegister(probStrech, SquashLatency)
  val bit = ShiftRegister(bit0, macs.last.latency + SquashLatency, macCE)
  val last = ShiftRegister(last0, macs.last.latency + SquashLatency, macCE)

  // Ctrl singls gen
  val cntCE = Wire(Bool())
  val (cnt, cntWrp) = Counter(0 until p.VecDotII, cntCE)

  val sIdle :: sWorking :: Nil = Enum(2)
  val stateNxt = Wire(UInt())
  val state = RegNext(stateNxt, sIdle)

  val takeInput = io.in.fire

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
  
  load := takeInput
  wxSel := cnt
  
  macReload := cnt === 0.U
  macCE := true.B
  
  output := ShiftRegister(cntWrp, macs.last.latency + SquashLatency, resetData = false.B, en = true.B)
  
  // outputs
  io.in.ready := inIdle || (inWorking && cntWrp)

  io.P.bits.bit := bit
  io.P.bits.prob := prob
  io.P.bits.last := last
  io.P.valid := output

  io.x.bits.x := x
  io.x.bits.bit := bit
  io.x.bits.last := last
  io.x.valid := output

  for(i <- 0 until p.VecDotMACNum) {
    io.updateStrm.bits.w(i) := macs(i).io.a
    io.updateStrm.bits.x(i) := macs(i).io.b
  }
  io.updateStrm.valid := inWorking
  when(inWorking) {
    assert(io.updateStrm.ready, "updateStrm FIFO full!")
  }

  val latency = p.VecDotII + macs.last.latency + SquashLatency
}

class LossCalPE()(implicit p : MixerParameter) extends Module {
  val latency = 1

  val io = IO(new Bundle {
    val P = Flipped(ValidIO(new BitProbBundle()))

    val loss = ValidIO(SInt(p.lossWidth))
  })
  // ctrl signals
  val lossLoad = Wire(Bool())
  val probLoad = Wire(Bool())
  
  // data path
  val prob = RegEnable(io.P.bits.prob, probLoad)
  val bit = RegEnable(io.P.bits.bit, probLoad)
  val probExpect = 0.U(p.lossWidth) | Cat(bit, 0.U(12.W))

  val lr = 31.U // TODO: Learning rate
  val lossCal = (probExpect - prob).asSInt * lr
  val loss = lossCal

  // ctrl signals gne
  lossLoad := io.P.valid
  probLoad := io.P.valid

  // output
  io.loss.bits := loss
  io.loss.valid := RegNext(io.P.valid, false.B)
}

class UpdatePE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val loss = Flipped(ValidIO(SInt(p.lossWidth)))
    val updateStrm = Flipped(DecoupledIO(new WeightUpdateBundle()))

    val wStrm = ValidIO(new WeightsWriteBackBundle())
  })

  // ctrl signals
  val cntCE = Wire(Bool())
  val (cnt, cntWrp) = Counter(0 until p.VecDotII, cntCE)

  val sIdle :: sWorking :: Nil = Enum(2)
  val stateNxt = Wire(UInt())
  val state = RegNext(stateNxt, sIdle)

  val takeInput = io.loss.valid

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

  // data path
  val loss = RegEnable(io.loss.bits, io.loss.valid)

  def getSign(x: SInt) = x(x.getWidth - 1)

  val mss = Seq.fill(p.VecScaleSubMSNum) {Module(new MS(p.XWidth, p.lossWidth, p.WeightWidth))}
  mss.zipWithIndex.foreach{case (m, i) =>
    m.io.ce := true.B
    m.io.a := io.updateStrm.bits.x(i)
    m.io.b := loss
    m.io.c := io.updateStrm.bits.w(i)

    val aSign = getSign(m.io.a)
    val bSign = getSign(m.io.b)
    val cSign = getSign(m.io.c)

    val mulSign = ShiftRegister(aSign ^ bSign, m.latency)
    val weightSign = ShiftRegister(cSign, m.latency)
    val sumSign = getSign(m.io.out)

    val overflow = ~mulSign & ~weightSign & sumSign
    val underflow = mulSign & weightSign & ~sumSign
    io.wStrm.bits.w(i) := Mux(overflow, 65535.S, Mux(underflow, -65536.S, m.io.out)) 
  }

  // io
  io.updateStrm.ready := inWorking
  io.wStrm.valid := ShiftRegister(inWorking, mss.last.latency, false.B, en = true.B)

  when(inWorking) {
    assert(io.updateStrm.valid, "FIFO empty!")
  }

  val latency = mss.last.latency
}
