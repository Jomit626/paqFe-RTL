package paqFe.util

import chisel3._
import chisel3.util._

class DecoupledRegSlice[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
  })
  val outValid = RegEnable(io.in.valid, false.B, io.in.ready)
  val inReady = io.out.ready || ~outValid
  val bits = RegEnable(io.in.bits, io.in.fire)

  io.in.ready := inReady
  io.out.bits := bits
  io.out.valid := outValid
}

object DecoupledRegSlice {
  def apply[T <: Data](in: DecoupledIO[T]) = {
    val m = Module(new DecoupledRegSlice(chiselTypeOf(in.bits)))
    m.io.in <> in
    m.io.out
  }
}
