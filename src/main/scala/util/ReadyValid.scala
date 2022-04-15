package paqFe.util

import chisel3._
import chisel3.util._

class DecoupledHalfRegSlice[T <: Data](gen: T) extends Module {
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

object DecoupledHalfRegSlice {
  def apply[T <: Data](in: DecoupledIO[T]) = {
    val m = Module(new DecoupledHalfRegSlice(chiselTypeOf(in.bits)))
    m.io.in <> in
    m.io.out
  }
}

class DecoupledRegSlice[T <: Data](gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
  })

  val inReady = RegInit(true.B)
  val outValid = RegInit(false.B)

  val sEmpty :: sWorking :: sFull :: Nil = Enum(3)
  val state = RegInit(sEmpty)
  val stateNxt = WireInit(state)
  state := stateNxt

  inReady := stateNxt =/= sFull
  outValid := stateNxt =/= sEmpty

  switch(state) {
    is(sEmpty) {
      when(io.in.valid) {
        stateNxt := sWorking
      }
    }

    is(sWorking) {
      when(io.in.valid && ~io.out.ready) {
        stateNxt := sFull
      }.elsewhen(~io.in.valid && io.out.ready) {
        stateNxt := sEmpty
      }
    }

    is(sFull) {
      when(io.out.ready) {
        stateNxt := sWorking
      }
    }
  }

  io.in.ready := inReady
  io.out.bits := RegEnable(io.in.bits, io.in.fire)
  io.out.valid := outValid
}

object DecoupledRegSlice {
  def apply[T <: Data](in: DecoupledIO[T]) = {
    val m = Module(new DecoupledRegSlice(chiselTypeOf(in.bits)))
    m.io.in <> in

    m.io.out
  }
}

object DecoupledSimpleGatter {
  def apply[T <: Data](in: Vec[DecoupledIO[T]]) : DecoupledIO[Vec[T]] = {
    val out = Wire(Decoupled(Vec(in.length, chiselTypeOf(in.head.bits))))
    out.bits := VecInit(in.map(_.bits))

    in.foreach(_.ready := out.fire) 
    out.valid := in.map(_.valid).reduce(_ && _)
    out
  }

  def apply[T1 <: Data, T2 <: Data](in: Vec[DecoupledIO[T1]], gatter: Vec[T1] => T2): DecoupledIO[T2] = {
    val outBits = gatter(VecInit(in.map(_.bits)))
    val out = Wire(DecoupledIO(chiselTypeOf(outBits)))
    out.bits := outBits

    in.foreach(_.ready := out.fire)
    out.valid := in.map(_.valid).reduce(_ && _)

    out
  }
}

object DecoupledMap {
  def apply[T1 <: Data, T2 <: Data](in: DecoupledIO[T1], f: T1 => T2) : DecoupledIO[T2] = {
    val outBits = f(in.bits)
    val out = Wire(Decoupled(chiselTypeOf(outBits)))

    out.bits := outBits
    out.valid := in.valid
    in.ready := out.ready
    
    out
  }
}
