package models

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.internal.VerilatorBackendAnnotation

class Order1Spec
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Order1"
  it should "i don't konw" in {

    val testData = Seq[Int](0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)

    val inputBundle = testData.map(
      byte =>
        Seq(new NibbleBundle().Lit(_.nibble -> ((byte >> 4) & 0xF).U), 
            new NibbleBundle().Lit(_.nibble -> ((byte >> 0) & 0xF).U))
    ) reduce (_ ++ _)

    test(new Order1)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>

      c.reset.poke(true.B)
      c.clock.step(16)
      c.reset.poke(false.B)
      c.clock.step(1)

      fork {
        c.io.in.valid.poke(false.B)
        for(x <- inputBundle) {
          c.clock.step(1)
          c.io.in.bits.poke(x)
          c.io.in.valid.poke(true.B)
        }
        c.clock.step(1)
        c.io.in.valid.poke(false.B)
        c.clock.step(8)
      }.join
    }
  }
}