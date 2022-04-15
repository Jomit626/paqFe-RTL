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
import paqFe.SpecClass

class DecoupledRegSliceSpec extends SpecClass {
  behavior of "DecoupledSkidBuf"
  it should "Operating without lossing data" in {
    test(new DecoupledSkidBuf(UInt(8.W))).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      val data = Seq.tabulate(16) {i => (i & 0xFF).U}
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)

      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      fork{
        // full speed
        dut.io.in.enqueueSeq(data)
        dut.clock.step(16)

        // full speed
        dut.io.in.enqueueSeq(data)
        dut.clock.step(16)

        // throttle
        for(d <- data) {
          dut.io.in.enqueue(d)
          dut.clock.step(Random.between(0, 4))
        }
        dut.clock.step(16)

        // throttle
        for(d <- data) {
          dut.io.in.enqueue(d)
          dut.clock.step(Random.between(0, 4))
        }
        dut.clock.step(16)

      }.fork {
        // full speed
        dut.io.out.expectDequeueSeq(data)
        dut.clock.step(16)

        // throttle
        for(d <- data) {
          dut.io.out.expectDequeue(d)
          dut.clock.step(Random.between(0, 4))
        }
        dut.clock.step(16)

        // full speed
        dut.io.out.expectDequeueSeq(data)
        dut.clock.step(16)

        // throttle
        for(d <- data) {
          dut.io.out.expectDequeue(d)
          dut.clock.step(Random.between(0, 4))
        }
        dut.clock.step(16)
      }.join()
    }
  }
}