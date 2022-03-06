package paqFe.mixer

import chisel3._
import chisel3.util._

class PredictUpdateEngineXCtrlBundle()(implicit p : MixerParameter) extends Bundle {
  val X = Output(XsBundle())
  val bit = Output(UInt(1.W))
  val harzardFastPath = Output(Bool())
}

object XsBundle {
  def apply()(implicit p : MixerParameter) = {
    Vec(p.nFeatures, SInt(p.XWidth))
  }
}

object WeightsBundle {
  def apply()(implicit p : MixerParameter) = {
    Vec(p.nFeatures, SInt(p.WeightWidth))
  }
}
