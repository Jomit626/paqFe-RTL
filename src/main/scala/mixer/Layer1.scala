package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.ram.RamInitUnit

class PredictUpdateEngine(forceFirstProbEven: Boolean = false)(implicit p : MixerParameter) extends Module {
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

class MixerLayer1PE(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Layer1InputBundle()))

    val x = ValidIO(XBitBundle())
    val status = Output(new StatusBundle())
  })

  // data path
  // to DotProduct PE
  val weightRam = Mem(256, Vec(p.nFeatures, SInt(p.WeightWidth)))
  val writeEn = Seq.fill(p.nFeatures) {WireInit(false.B)}
  val writeAddr = Wire(UInt(8.W))
  val writeData = Wire(Vec(p.nFeatures, SInt(p.WeightWidth)))

  val ramInit = Module(new RamInitUnit(8))
  ramInit.io.in.bits := io.in.bits.last
  ramInit.io.in.valid := io.in.fire

  when(ramInit.io.status.initDone) {
    weightRam.write(writeAddr, writeData, writeEn)
  }.otherwise {
    weightRam.write(ramInit.io.waddr, VecInit.fill(p.nFeatures)(p.L1WeightInitVal.S) , Seq.fill(p.nFeatures) {ramInit.io.wen})
  }

  val pe = Module(new PredictUpdateEngine())
  pe.io.in.bits.w := weightRam.read(io.in.bits.ctx)
  pe.io.in.bits.x := io.in.bits.x
  pe.io.in.bits.bit := io.in.bits.bit

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

  for(i <- 0 until n) {
    val loadPrev = if(i != 0 ) (!shiftInFlightValid(i) && shiftInFlightValid(i - 1)) else false.B
    val feedNext = if(i != n - 1) (shiftInFlightValid(i) && !shiftInFlightValid(i + 1)) else false.B

    if(i == 0) {
      when(io.in.fire) {
        shiftInFlightCtxs.head := io.in.bits.ctx
        shiftInFlightValid.head := true.B
      }
    } else {
      when(loadPrev) {
        shiftInFlightCtxs(i) := shiftInFlightCtxs(i - 1)
      }
    }

    when(loadPrev && feedNext) {
      shiftInFlightValid(i) := true.B
    }.elsewhen(loadPrev) {
      shiftInFlightValid(i) := true.B
    }.elsewhen(feedNext) {
      shiftInFlightValid(i) := false.B
    }


  }

  when(shiftInFlightCtxsDeq) {
    assert(shiftInFlightValid.last)
    shiftInFlightValid.last := false.B
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
}

class MixerLayer1(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MixerInputBundle))

    val out = Vec(p.nHidden, ValidIO(new XBitBundle()))
    val status = Output(new StatusBundle())
  })

  val pes = Seq.fill(p.nHidden) {Module(new MixerLayer1PE)}
  val allReady = pes.map(_.io.in.ready).reduce(_ && _)
  pes.zipWithIndex.foreach{ case(m, i) =>

    m.io.in.bits.bit := io.in.bits.bit
    m.io.in.bits.last := io.in.bits.last
    m.io.in.bits.ctx := io.in.bits.ctx(i)
    m.io.in.bits.x := io.in.bits.x
    m.io.in.valid := io.in.valid && allReady

    io.out(i) := m.io.x
  }

  io.in.ready := allReady

  io.status := StatusMerge(pes.map(_.io.status))
}
