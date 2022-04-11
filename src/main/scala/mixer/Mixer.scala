package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types.BitProbBundle
import paqFe.types.StatusBundle

class BitLastBundle extends Bundle {
  val bit = UInt(1.W)
  val last = Bool()
}

class Mixer(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MixerInputBundle))

    val out = DecoupledIO(new BitProbBundle())
    val status = Output(new StatusBundle)
  })
  val stall = Wire(Bool())

  val l1 = Module(new MixerLayer1)
  val l2 = Module(new MixerLayer2)

  l1.io.in.bits := io.in.bits
  l1.io.in.valid := io.in.valid && ~stall
  io.in.ready := l1.io.in.ready && ~stall

  val mergedL1 = Wire(chiselTypeOf(l2.io.in))

  // TODO: shift regs will be batter?
  val valid = l1.io.out.zipWithIndex.map{case (out, i) =>
    val q = Module(new Queue(SInt(p.XWidth), 8))

    q.io.enq.valid := out.valid
    q.io.enq.bits := out.bits.x
    when(out.valid) {
      assert(q.io.enq.ready, "FIFO full")
    }

    mergedL1.bits.x(i) := q.io.deq.bits
    q.io.deq.ready := mergedL1.ready
    q.io.deq.valid
  }.reduce(_ && _)

  val bitLastQueue = Module(new Queue(new BitLastBundle, 2))
  bitLastQueue.io.enq.valid := l1.io.out.head.valid
  bitLastQueue.io.enq.bits.bit := l1.io.out.head.bits.bit
  bitLastQueue.io.enq.bits.last := l1.io.out.head.bits.last

  mergedL1.bits.bit := bitLastQueue.io.deq.bits.bit
  mergedL1.bits.last := bitLastQueue.io.deq.bits.last
  bitLastQueue.io.deq.ready := mergedL1.ready

  mergedL1.valid := valid && bitLastQueue.io.deq.valid
  
  l2.io.in <> mergedL1

  val outputQueueSize = 16
  val n = (l1.latency + l2.latency + p.VecDotII - 1) / p.VecDotII
  val outputQueue = Module(new Queue(new BitProbBundle(), outputQueueSize))
  stall := outputQueue.io.count > (outputQueueSize - n).U

  outputQueue.io.enq.valid := l2.io.out.valid
  outputQueue.io.enq.bits := l2.io.out.bits
  when(l2.io.out.valid) {
    assert(outputQueue.io.enq.ready, "FIFO full!")
  }

  io.out <> outputQueue.io.deq
  io.status := l1.io.status
}