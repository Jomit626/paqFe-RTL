package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.util._

class PredictUpdateEngine()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new VecDotPackBundle()))

    val P = ValidIO(new BitProbBundle())
    val x = ValidIO(XBitBundle())

    val Wu = ValidIO(new WeightsWriteBackBundle())
  })

  val predictPE = Module(new PredictPE)
  val lossPE = Module(new LossCalPE)
  val updatePE = Module(new UpdatePE)
  
  val latency = predictPE.latency + lossPE.latency + updatePE.latency

  predictPE.io.in <> io.in
  
  lossPE.io.P <> predictPE.io.P
  io.P <> predictPE.io.P
  io.x <> predictPE.io.x

  updatePE.io.loss := lossPE.io.loss
  updatePE.io.updateStrm <> Queue(predictPE.io.updateStrm, p.VecDotII + predictPE.latency + lossPE.latency + 1)

  io.Wu <> updatePE.io.wStrm
}

class Layer1InputBundle(implicit p: MixerParameter) extends Bundle {
  val x = Vec(p.nFeatures, SInt(p.XWidth))
  val ctx = UInt(8.W)
  val bit = UInt(1.W)
  val last = Bool()
}

class MixerInputBundle(implicit p: MixerParameter) extends Bundle {
  val x = Vec(p.nFeatures, SInt(p.XWidth))
  val ctx = Vec(p.nHidden, UInt(8.W))
  val bit = UInt(1.W)
  val last = Bool()
}

class MixerInputXBundle(implicit p: MixerParameter) extends Bundle {
  val x = Vec(p.nFeatures, SInt(p.XWidth))
  val bit = UInt(1.W)
  val last = Bool()
}

class MixerInputProbBundle(implicit p: MixerParameter) extends Bundle {
  val probs = Vec(p.nFeatures, UInt(12.W))
  val bit = UInt(1.W)
  val last = Bool()
}

class MixerLayer1PE(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Layer1InputBundle()))

    val x = ValidIO(XBitBundle())
    val status = Output(new StatusBundle())
  })

  // data path
  // to DotProduct PE
  val weightRam = Mem(256, Vec(p.nFeatures, SInt(p.WeightWidth))) // TODO: vivado failed to infer ram
  val writeEn = Seq.fill(p.nFeatures) {WireInit(false.B)}
  val writeAddr = Wire(UInt(8.W))
  val writeData = Wire(Vec(p.nFeatures, SInt(p.WeightWidth)))

  val ramInit = Module(new RamInitUnit(8))
  ramInit.io.in.bits := io.in.bits.last
  ramInit.io.in.valid := io.in.fire

  val wen = Seq.tabulate(p.nFeatures) {i => Mux(ramInit.io.status.initDone, writeEn(i), ramInit.io.wen)}
  val waddr = Mux(ramInit.io.status.initDone, writeAddr, ramInit.io.waddr)
  val wdata = Mux(ramInit.io.status.initDone, writeData, VecInit.fill(p.nFeatures)(p.L1WeightInitVal))
  weightRam.write(waddr, wdata, wen)

  val pe = Module(new PredictUpdateEngine())
  pe.io.in.bits.w := weightRam.read(io.in.bits.ctx)
  pe.io.in.bits.x := io.in.bits.x
  pe.io.in.bits.bit := io.in.bits.bit
  pe.io.in.bits.last := io.in.bits.last

  // from DotProduct PE to weightRam
  for(j <- 0 until p.VecScaleSubRound) {
    for(i <- 0 until p.VecScaleSubMSNum) {
      if(j * p.VecScaleSubMSNum + i < p.nFeatures)
        writeData(j * p.VecScaleSubMSNum + i) := pe.io.Wu.bits.w(i)
    }
  }

  // ctrl singals

  // harzard
  val n = (pe.latency + p.VecDotII - 1) / p.VecDotII
  val shiftInFlightCtxs = Seq.fill(n) {Reg(UInt(8.W))}
  val shiftInFlightValid = Seq.fill(n) {RegInit(false.B)}
  val shiftInFlightCtxsDeq = WireInit(false.B)
  val feedNext = Seq.tabulate(n) {i => 
    if(i != n - 1) {
      shiftInFlightValid(i) && (~shiftInFlightValid.slice(i, n).reduce(_ & _) || shiftInFlightCtxsDeq)
    } else {
      shiftInFlightCtxsDeq
    }
  }
  val loadPrev = Seq.tabulate(n) {i => 
    if(i != 0) {
      feedNext(i - 1)
    } else {
      io.in.fire
    }
  }
  for(i <- 0 until n) {
    when(loadPrev(i)) {
      shiftInFlightCtxs(i) := (if (i == 0) io.in.bits.ctx else shiftInFlightCtxs(i - 1))
    }

    when(loadPrev(i) && feedNext(i)) {
      shiftInFlightValid(i) := true.B
    }.elsewhen(loadPrev(i)) {
      shiftInFlightValid(i) := true.B
    }.elsewhen(feedNext(i)) {
      shiftInFlightValid(i) := false.B
    }
  }

  when(shiftInFlightCtxsDeq) {
    assert(shiftInFlightValid.last)
  }

  val harzard = Seq.tabulate(n) {i => 
    shiftInFlightValid(i) && shiftInFlightCtxs(i) === io.in.bits.ctx
  }.reduce(_ || _)

  io.in.ready := !harzard && pe.io.in.ready
  pe.io.in.valid := !harzard && io.in.valid

  // weight write
  writeAddr := shiftInFlightCtxs.last
  val writeBack = pe.io.Wu.valid
  val writeBackRound = Seq.tabulate(p.VecScaleSubRound) {i => RegInit(( i == 0).B)}
  writeBackRound.zipWithIndex foreach {case (r, i) => 
    when(writeBack) {
      r := writeBackRound((i - 1 + writeBackRound.length) % writeBackRound.length)
    }
  }

  for(j <- 0 until p.VecScaleSubRound) {
    for(i <- 0 until p.VecScaleSubMSNum) {
      if(j * p.VecScaleSubMSNum + i < p.nFeatures) {
        writeEn(j * p.VecScaleSubMSNum + i) := writeBackRound(j) && writeBack
      }
    }
  }

  shiftInFlightCtxsDeq := writeBack && writeBackRound.last

  io.x := pe.io.x
  io.status := ramInit.io.status

  val latency = pe.latency
}

class MixerLayer1(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MixerInputBundle))

    val out = Vec(p.nHidden, ValidIO(new XBitBundle()))
    val status = Output(new StatusBundle())
  })

  val PEs = Seq.fill(p.nHidden) {Module(new MixerLayer1PE)}
  val PEsIn = Seq.fill(p.nHidden) {Wire(chiselTypeOf(PEs.head.io.in))}

  val allReady = PEsIn.map(_.ready).reduce(_ && _)

  for(i <- 0 until PEs.length) {
    PEsIn(i).bits.bit := io.in.bits.bit
    PEsIn(i).bits.last := io.in.bits.last
    PEsIn(i).bits.ctx := io.in.bits.ctx(i)
    PEsIn(i).bits.x := io.in.bits.x
    PEsIn(i).valid := io.in.valid && allReady

    PEs(i).io.in <> DecoupledFullRegSlice(PEsIn(i))

    io.out(i) := PEs(i).io.x
  }

  io.in.ready := allReady

  io.status := StatusMerge(PEs.map(_.io.status))
  
  val latency = PEs.last.latency
}
