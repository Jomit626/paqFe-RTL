package paqFe.models

import chisel3._

object TabHash {
  def apply(x: UInt, hashTab: Seq[Seq[Int]]) = {
    Seq.tabulate((x.getWidth + 7) / 8) { i =>
      val byte = x((i * 8 + 7) min (x.getWidth - 1), i * 8)
      VecInit(hashTab(i).map(_.U))(byte)
    }.reduce(_ ^ _)
  }
}
