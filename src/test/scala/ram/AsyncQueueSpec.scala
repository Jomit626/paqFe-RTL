package paqFe.ram

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import scala.util.Random

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SyncQueue[T <: Data](gen: T, Depth: Int) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))

    val deq = DecoupledIO(gen)
  })

  val inst = Module(new AsyncQueue(gen, Depth))
  inst.io.enq_clock := clock
  inst.io.enq_reset := reset
  inst.io.enq <> io.enq

  inst.io.deq_clock := clock
  inst.io.deq_reset := reset
  inst.io.deq <> io.deq
}

class AsyncQueueSpec extends AnyFlatSpec
  with ChiselScalatestTester {
  
  behavior of "AsyncQueue"
  it should "Operating without lossing data" in {
    test(new SyncQueue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      val data = Seq.tabulate(4096) {i => (i & 0xFF).U}
      dut.io.enq.initSource()
      dut.io.enq.setSourceClock(dut.clock)

      dut.io.deq.initSink()
      dut.io.deq.setSinkClock(dut.clock)

      fork{
        // full speed
        dut.io.enq.enqueueSeq(data)
        dut.clock.step(128)

        // full speed
        dut.io.enq.enqueueSeq(data)
        dut.clock.step(128)

        // throttle
        for(d <- data) {
          dut.io.enq.enqueue(d)
          dut.clock.step(Random.between(0, 64))
        }
        dut.clock.step(128)

        // throttle
        for(d <- data) {
          dut.io.enq.enqueue(d)
          dut.clock.step(Random.between(0, 64))
        }
        dut.clock.step(128)

      }.fork {
        // full speed
        dut.io.deq.expectDequeueSeq(data)
        dut.clock.step(128)

        // throttle
        for(d <- data) {
          dut.io.deq.expectDequeue(d)
          dut.clock.step(Random.between(0, 64))
        }
        dut.clock.step(128)

        // full speed
        dut.io.deq.expectDequeueSeq(data)
        dut.clock.step(128)

        // throttle
        for(d <- data) {
          dut.io.deq.expectDequeue(d)
          dut.clock.step(Random.between(0, 64))
        }
        dut.clock.step(128)
      }.join()
    }
  }
}