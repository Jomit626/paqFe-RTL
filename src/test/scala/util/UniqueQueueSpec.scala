package paqFe.util

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import scala.util.Random

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniqueQueueSpec extends AnyFlatSpec
  with ChiselScalatestTester {
  
  behavior of "UniqueQueue"
  it should "Operating without lossing data" in {
    test(new UniqueQueue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      val data = Seq.tabulate(512) {i => (i & 0xFF).U}
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

  it should "Not allow same elem in queue" in {
    test(new UniqueQueue(UInt(8.W), 8)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.io.enq.initSource()
      dut.io.enq.setSourceClock(dut.clock)

      dut.io.deq.initSink()
      dut.io.deq.setSinkClock(dut.clock)

      dut.io.enq.enqueue(1.U)
      dut.io.enq.enqueue(2.U)
      dut.io.enq.enqueue(3.U)
      dut.clock.step()

      dut.io.enq.bits.poke(1.U)
      dut.io.enq.ready.expect(false.B)
      dut.clock.step()

      dut.io.enq.bits.poke(4.U)
      dut.io.enq.ready.expect(true.B)
      dut.clock.step()

      dut.io.deq.expectDequeue(1.U)

      dut.io.enq.bits.poke(1.U)
      dut.io.enq.ready.expect(true.B)
    }
  }
}