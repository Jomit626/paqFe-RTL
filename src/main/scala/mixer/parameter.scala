package paqFe.mixer

import chisel3._

class MixerParameter(val nFeatures: Int = 75,  val nHidden: Int = 8) {
 
  val WeightWidth = 17.W
  val XWidth = 12.W
  val lossWidth = 18.W

  val VecDotMACNum = 8
  val VecDotII = (nFeatures + VecDotMACNum - 1) / VecDotMACNum
  
  val VecScaleSubMSNum = VecDotMACNum
  val VecScaleSubRound = VecDotII
  
  val L1WeightInitVal = (1 << (WeightWidth.get + 1)) / nFeatures
  val L2WeightInitVal = (1 << 16) / nHidden

  val L2BatchSize = 1024
}
