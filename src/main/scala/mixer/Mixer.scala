package paqFe.mixer

import chisel3._
import chisel3.util._

import paqFe.types.BitProbBundle
import paqFe.types.StatusBundle
import paqFe.util.DecoupledSimpleGatter

class BitLastBundle extends Bundle {
  val bit = UInt(1.W)
  val last = Bool()
}

class Mixer(forceFirstProbEven: Boolean = false)(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new MixerInputBundle))

    val out = DecoupledIO(new BitProbBundle())
    val status = Output(new StatusBundle)
  })
  val stall = Wire(Bool())

  val l1 = Module(new MixerLayer1)
  val l2 = Module(new MixerLayer2(forceFirstProbEven))

  l1.io.in.bits := io.in.bits
  l1.io.in.valid := io.in.valid && ~stall
  io.in.ready := l1.io.in.ready && ~stall

  val l1tol2Queues = VecInit(l1.io.out.map{case out =>
    val q = Module(new Queue(new XBitBundle(), 8))

    q.io.enq.valid := out.valid
    q.io.enq.bits := out.bits
    when(out.valid) {
      assert(q.io.enq.ready, "FIFO full")
    }

    q.io.deq
  })

  def layer1ToLayer2(in: Vec[XBitBundle]): Layer2InputBundle = {
    val out = Wire(new Layer2InputBundle())
    for(i <- 0 until out.x.length) {
      out.x(i) := in(i).x
    }

    out.bit := in.head.bit
    out.last := in.head.last
    out
  }

  l2.io.in <> DecoupledSimpleGatter(l1tol2Queues, layer1ToLayer2)

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