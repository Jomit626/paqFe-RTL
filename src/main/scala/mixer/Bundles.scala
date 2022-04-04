package paqFe.mixer

import chisel3._
import chisel3.util._

class PredictUpdateEngineXCtrlBundle()(implicit p : MixerParameter) extends Bundle {
  val X = XsBundle()
  val bit = UInt(1.W)
}

class XBitBundle()(implicit p : MixerParameter) extends Bundle {
  val x = SInt(p.XWidth)
  val bit = UInt(1.W)
}

object XBitBundle {
  def apply()(implicit p : MixerParameter) = new XBitBundle()
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

class WeightUpdateBundle()(implicit p : MixerParameter) extends Bundle {
  val w = Vec(p.VecDotMACNum, SInt(p.WeightWidth))
  val x = Vec(p.VecDotMACNum, SInt(p.XWidth))
}

class WeightsWriteBackBundle()(implicit p : MixerParameter) extends Bundle {
  val w = Vec(p.VecDotMACNum, SInt(p.WeightWidth))
}

class Layer2InputBundle(implicit p: MixerParameter) extends Bundle {
  val x = Vec(p.nHidden, SInt(p.XWidth))
  val bit = UInt(1.W)
}
