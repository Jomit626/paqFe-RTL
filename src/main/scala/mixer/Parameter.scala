package paqFe.mixer

import chisel3._

class MixerParameter(val nFeatures: Int = 75,  val nHidden: Int = 8) {
  val WeightWidth = 17.W
  val XWidth = 12.W
  val lossWidth = 18.W

  val VecDotMACNum = 4
  val VecDotII = (nFeatures + VecDotMACNum - 1) / VecDotMACNum
  
  val VecScaleSubMSNum = VecDotMACNum
  val VecScaleSubRound = VecDotII
  
  def L1WeightInitVal = ((1 << 16) / nFeatures - 1).S(WeightWidth)
  def L2WeightInitVal = ((1 << 16) / nHidden - 1).S(WeightWidth)

  val L2BatchSize = 1024
}
