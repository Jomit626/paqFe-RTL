package paqFe.mixer

import chisel3._

class MixerParameter(n : Int = 4) {
  val nFeatures = n

  val WeightWidth = 17.W
  val XWidth = 12.W
  val lossWidth = 18.W

  val WeightInitVal = (1 << (WeightWidth.get + 1)) / n
}
