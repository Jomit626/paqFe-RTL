package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types._

class PredictUpdateEngine()(implicit p : MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new PredictUpdateEngineXCtrlBundle()))
    val W = Flipped(DecoupledIO(WeightsBundle()))

    val P = ValidIO(new BitProbBundle())
    val Wu = ValidIO(SInt(p.WeightWidth))
  })

  val predictPE = Module(new PredictPE)
  val lossPE = Module(new LossCalPE)
  val updatePE = Module(new UpdatePE)
  
  
  predictPE.io.W <> io.W
  predictPE.io.in <> io.in
  
  lossPE.io.P <> predictPE.io.P
  io.P <> predictPE.io.P

  updatePE.io.loss := lossPE.io.loss
  updatePE.io.updateStrm <> Queue(predictPE.io.updateStrm, p.VecDotII + predictPE.latency + lossPE.latency + 1)

  io.Wu <> updatePE.io.wStrm
}