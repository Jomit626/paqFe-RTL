package models

import chisel3._
import org.scalatest._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.internal.VerilatorBackendAnnotation
import verifydata._

import scala.io.Source
import java.io._
import com.github.tototoshi.csv._

import types._

class Order1Spec
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Order1"

  val db = new VerifyData("../paqFe/verify/db/order1-db")
  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "work with " + test_name in {
      println("read", input_file, "test", output_file)
      val is = new FileInputStream(input_file)
      val reader = CSVReader.open(new File(output_file))

      val bitProbs = reader.all().map{line => (line(0).toInt, line(1).toInt)}.toIndexedSeq
      assert(bitProbs.length % 8 == 0)

      test(new Order1)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      
      c.io.in.initSource()
      c.io.in.setSourceClock(c.clock)
      for(i <- 0 until 8) {
        c.io.out(i).initSink()
        c.io.out(i).setSinkClock(c.clock)
      }

      c.reset.poke(true.B)
      c.clock.step(16)
      c.reset.poke(false.B)
      c.io.in.valid.poke(false.B)
      c.clock.step()
      
      // input
      var thr = fork {
        val buf = new Array[Byte](4)

        while(is.read(buf) > 0) {
          for(byte <- buf) {
            c.io.in.valid.poke(true.B)
            c.io.in.bits.nibble.poke(((byte >> 4) & 0xF).U)
            c.clock.step()
            c.io.in.valid.poke(true.B)
            c.io.in.bits.nibble.poke((byte & 0xF).U)
            c.clock.step()
            c.io.in.valid.poke(false.B)
          }
        }
      }
      
      for(i <- 0 until 8) {
        val exp = new BitProbBundle()
        thr = thr.fork {
          for(idx <- 0 until(bitProbs.length / 8)) {
            val (bit, prob) = bitProbs(idx * 8 + i)

            c.io.out(i).expectDequeue(exp.Lit(
              _.bit -> bit.U,
              _.prob -> prob.U
            ))
          }
        }
      }
      
      thr.join()
    }
    }
  }
  
}