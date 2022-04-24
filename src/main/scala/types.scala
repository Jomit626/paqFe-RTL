package paqFe.types

import chisel3._
import chisel3.util._

import paqFe.util._

class ByteIdxBundle extends Bundle {
  val idx = UInt(8.W)
  val byte = UInt(8.W)
  val last = Bool()
}

class CompressorOutputBundle extends Bundle {
  val data = UInt(16.W)
  val last = Bool()
}

class ByteBundle extends Bundle {
  val byte = UInt(8.W)
  val last = Bool()
}

class NibbleBundle extends Bundle {
  val byte = UInt(8.W)
  val nibble = UInt(4.W)
  val last = Bool()
}

class CtxBundle(CtxWidth : Int) extends Bundle {
  val context = UInt(CtxWidth.W)
  val last = Bool()
}

class NibbleCtxBundle(CtxWidth : Int) extends Bundle {
  val nibble = UInt(4.W)
  val context = UInt(CtxWidth.W)
  val chk = UInt(8.W)
  val last = Bool()
}

class BitProbBundle extends Bundle {
  val bit = UInt(1.W)
  val prob = UInt(12.W)
  val last = Bool()
}

class BitBundle extends Bundle {
  val bit = UInt(1.W)
  val last = Bool()
}

class BitProbsCtxBundle(n : Int) extends Bundle {
  val bit = UInt(1.W)
  val ctx = UInt(8.W)
  val probs = Vec(n, UInt(12.W))
  val last = Bool()
}

class StatusBundle extends Bundle {
  val initDone = Bool()
}

object StatusMerge {
  def apply(seq : Seq[StatusBundle]) = {
    TreeReduce(seq, (a : StatusBundle,b : StatusBundle) => {
      val c = Wire(new StatusBundle)
      c.initDone := a.initDone && b.initDone

      c
    })
  }
}
