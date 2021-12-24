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
import LocalHelpers._
import firrtl.PrimOps

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
      test(new Order1)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      
        testInit(c)
        testOrder1(c, input_file, output_file)

      } 
    }
  }

  it should "work " in {
    test(new Order1)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      testInit(c)
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        testOrder1(c, input_file, output_file)
        waitOrder1InitDone(c)
      }
    }
  }
  
}


private object LocalHelpers {
  def waitOrder1InitDone(c : Order1) {
    var done = false
    while(!done) {
      c.clock.step()
      done = c.io.status.initDone.peek().litToBoolean
    }
  }

  def testInit(c : Order1) {
    c.clock.setTimeout(1000 + (1 << 12))
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

    waitOrder1InitDone(c)
  }

  def testOrder1(c : Order1, input_file : String, output_file : String) = {
    val is = new FileInputStream(input_file)
    val reader = CSVReader.open(new File(output_file))

    val bitProbs = reader.all().map{line => (line(0).toInt, line(1).toInt)}.toIndexedSeq
    assert(bitProbs.length % 8 == 0)

    var thr = fork {
      val buf = new Array[Byte](1024)

      var n = is.read(buf)
      while(n > 0) {
        val last = is.available() == 0;
        for(i <- 0 until n) {
          val byte = buf(i)

          c.io.in.valid.poke(true.B)
          c.io.in.bits.last.poke((last && (i == (n - 1))).B)
          c.io.in.bits.nibble.poke(((byte >> 4) & 0xF).U)
          c.clock.step()

          c.io.in.valid.poke(true.B)
          c.io.in.bits.last.poke((last && (i == (n - 1))).B)
          c.io.in.bits.nibble.poke((byte & 0xF).U)
          c.clock.step()

          c.io.in.valid.poke(false.B)
          c.io.in.bits.last.poke(false.B)
        }
        n = is.read(buf)
      }
      c.clock.step(16)
    }
    
    for(i <- 0 until 8) {
      val exp = new BitProbBundle()
      thr = thr.fork {
        for(idx <- 0 until (bitProbs.length / 8)) {
          val (bit, prob) = bitProbs(idx * 8 + i)

          c.io.out(i).expectDequeue(exp.Lit(
            _.bit -> bit.U,
            _.prob -> prob.U,
            _.last -> (idx == (bitProbs.length / 8 - 1)).B
          ))
        }
      }
    }
    
    thr.join()
  }
}