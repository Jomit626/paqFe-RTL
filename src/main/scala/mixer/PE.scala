package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._

class PredictPE()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new PredictUpdateEngineXCtrlBundle()))
    val W = Flipped(DecoupledIO(WeightsBundle()))

    val P = ValidIO(new BitProbBundle())  // TODO: split L1 and L2

    val updateStrm = DecoupledIO(new WeightUpdateBundle())
  })

  // ctrl signals
  val xLoad = Wire(Bool())
  val wLoad = Wire(Bool())
  val bitLoad = Wire(Bool())

  val wxSel = Wire(UInt(log2Ceil(p.VecDotII + 1).W))

  val macReload = Wire(Bool())
  val macCE = Wire(Bool())

  val pOut = Wire(Bool())

  // data path
  val bit0 = RegEnable(io.in.bits.bit, bitLoad)
  // Registers to store input W and X
  val WReg = Reg(Vec(p.nFeatures, SInt(p.WeightWidth)))
  val XReg = Reg(Vec(p.nFeatures, SInt(p.XWidth)))

  when(xLoad) {
    for(i <- 0 until p.nFeatures) {
      XReg(i) := io.in.bits.X(i)
    }
  }

  when(wLoad) {
    for(i <- 0 until p.nFeatures) {
      WReg(i) := io.W.bits(i)
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
  val SquashLatency = 1
  val dot = TreeReduce[SInt](macs.map(_.io.acc), (a, b) => a + b)
  val probStrech = dot >> 16
  val prob = RegNext(Squash(probStrech))
  val bit = ShiftRegister(bit0, macs.last.latency + SquashLatency, macCE)

  // Ctrl singls gen
  val cntCE = Wire(Bool())
  val (cnt, cntWrp) = Counter(0 until p.VecDotII, cntCE)

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
  
  macReload := cnt === 0.U
  macCE := true.B
  
  pOut := ShiftRegister(cntWrp, macs.last.latency + SquashLatency)
  
  // outputs
  io.in.ready := inIdle || (inWorking && cntWrp)
  io.W.ready := io.in.ready

  io.P.bits.bit := bit
  io.P.bits.prob := prob
  io.P.bits.last := false.B //DontCare
  io.P.valid := pOut

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

  val lr = 10.U(6.W)
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
  val state = RegNext(stateNxt)

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
  val loss = RegNext(io.loss.bits, io.loss.valid)

  val mss = Seq.fill(p.VecScaleSubMSNum) {Module(new MS(p.XWidth, p.lossWidth, p.WeightWidth))}
  mss.zipWithIndex.foreach{case (m, i) =>
    m.io.ce := true.B
    m.io.a := io.updateStrm.bits.x(i)
    m.io.b := loss
    m.io.c := io.updateStrm.bits.w(i)

    io.wStrm.bits.w(i) := m.io.out
  }

  // io
  io.updateStrm.ready := inWorking
  io.wStrm.valid := ShiftRegister(io.updateStrm.valid, mss.last.latency, false.B, en = true.B)

  when(inWorking) {
    assert(io.updateStrm.valid, "FIFO empty!")
  }
}
