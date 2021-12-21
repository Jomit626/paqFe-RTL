package types

import chisel3._
import chisel3.util._

class ByteBundle extends Bundle {
  val byte = Output(UInt(8.W))
  val last = Output(Bool())
}

class NibbleBundle extends Bundle {
  val nibble = Output(UInt(4.W))
  val last = Output(Bool())
}

class CtxBundle(CtxWidth : Int) extends Bundle {
  val context = Output(UInt(CtxWidth.W))
  val last = Output(Bool())
}

class NibbleCtxBundle(CtxWidth : Int) extends Bundle {
  val nibble = Output(UInt(4.W))
  val context = Output(UInt(CtxWidth.W))
  val last = Output(Bool())
}

class BitProbBundle extends Bundle {
  val bit = Output(UInt(1.W))
  val prob = Output(UInt(12.W))
  val last = Output(Bool())
}

class StatusBundle extends Bundle {
  val initDone = Output(Bool())
}
