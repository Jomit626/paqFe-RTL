package paqFe.util

import chisel3._
import chisel3.util._

class UniqueQueue[T <: Data](gen: T, Depth: Int) extends Module {
  val io = IO(new Bundle{
    val enq = Flipped(DecoupledIO(gen))
    val deq = DecoupledIO(gen)
  })
  // ctrl singal
  val rptr = Counter(Depth)
  val wptr = Counter(Depth)
  val maybeFull = RegInit(false.B)

  val doEnq = io.enq.fire
  val doDeq = io.deq.fire

  // data path
  val mem = Reg(Vec(Depth, gen))
  val memValid = RegInit(VecInit.fill(Depth) {false.B})

  when(doEnq) { 
    mem(wptr.value) := io.enq.bits
    memValid(wptr.value) := true.B
    wptr.inc()
  }

  when(doDeq) {
    memValid(rptr.value) := false.B
    rptr.inc()
  }
  
  // ctrl singal gen
  when(doEnq =/= doDeq) {
    maybeFull := doEnq
  }
  val full = wptr.value === rptr.value && maybeFull
  val empty = wptr.value === rptr.value && ~maybeFull

  val equal = mem zip memValid map {case (x, v) =>
    v && x.asUInt === io.enq.bits.asUInt
  }
  val harzard = equal.reduce(_ || _)
  
  io.deq.bits := mem(rptr.value)
  io.deq.valid := !empty
  io.enq.ready := !full && !harzard
}
