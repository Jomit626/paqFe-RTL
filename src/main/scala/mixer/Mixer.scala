package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types.BitProbBundle
import paqFe.types.StatusBundle

class Mixer(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MixerInputBundle))

    val out = ValidIO(new BitProbBundle())
    val status = Output(new StatusBundle)
  })

  val l1 = Module(new MixerLayer1)
  val l2 = Module(new MixerLayer2)

  l1.io.in <> io.in
  val mergedL1 = Wire(chiselTypeOf(l2.io.in))

  // TODO: shift regs will be batter?
  val valid = l1.io.out.zipWithIndex.map{case (out, i) =>
    val q = Module(new Queue(SInt(p.XWidth), 2))

    q.io.enq.valid := out.valid
    q.io.enq.bits := out.bits.x
    when(out.valid) {
      assert(q.io.enq.ready, "FIFO full")
    }

    mergedL1.bits.x(i) := q.io.deq.bits
    q.io.deq.ready := mergedL1.ready
    q.io.deq.valid
  }.reduce(_ && _)

  val qBit = Module(new Queue(UInt(1.W), 2))
  qBit.io.enq.valid := l1.io.out.head.valid
  qBit.io.enq.bits := l1.io.out.head.bits.bit

  mergedL1.bits.bit := qBit.io.deq.bits
  qBit.io.deq.ready := mergedL1.ready

  mergedL1.valid := valid && qBit.io.deq.valid

  l2.io.in <> mergedL1
  io.out := l2.io.out
  io.status := l1.io.status
}