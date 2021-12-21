package coder

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

class ArithCoderBehavior {
  var low : Long = 0
  var high : Long = 0xFFFFFFFF

  def encode(bit : Byte, prob : Long) : (Byte, Boolean) = {
    var output : Boolean = false

    val range = high - low
    val mul = range * prob
    val mid = low + (mul >> 12)

    if (bit == 1)
      high = mid
    else
      low = mid + 1

    if (((high ^ low) >> 24) == 0) {
      output = true

      high = ((high << 8) | 0xFF) & 0xFFFFFFFF
      low = (low << 8) & 0xFFFFFFFF
    }

    (((high >> 24) & 0xFF).toByte, output)
  }
}
import LocalHelpers._

class ArithCoderSpec
    extends FlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "ArithCoder"

  val db = new VerifyData("../paqFe/verify/db/coder-db")
  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "work with " + test_name in {
      println("read", input_file, "test", output_file)
      val is = new FileInputStream(input_file)
      val reader = CSVReader.open(new File(output_file))

      val outputData = reader.all().map(_(0).toInt)

      test(new ArithCoder)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      
        initCoderTest(c)
        testCoder(c, input_file, output_file)

      }
    }
  }
  it should "work " in {
    test(new ArithCoder)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      initCoderTest(c)
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        testCoder(c, input_file, output_file)
        c.clock.step(4)
      }
    }
  }
}

private object LocalHelpers {
  def initCoderTest(c : ArithCoder) {
    c.io.out.initSink()
    c.io.out.setSinkClock(c.clock)

    c.reset.poke(true.B)
    c.clock.step(16)
    c.reset.poke(false.B)
    c.io.in.valid.poke(false.B)
    c.clock.step()
  }

  def testCoder(c : ArithCoder, input_file : String, output_file : String) = {
    val is = new FileInputStream(input_file)
    val reader = CSVReader.open(new File(output_file))

    val outputData = reader.all().map(_(0).toInt)

    fork {
      val buf = new Array[Byte](1024)
      var prob = 2048
      
      var n = is.read(buf)
      while(n > 0) {
        val last = is.available() == 0;
        for(i <- 0 until n) {
          val byte = buf(i)
          for(j <- 0 until 8) {
            c.io.in.valid.poke(true.B)
            c.io.in.bits.bit.poke(((byte >> (7 - j)) & 0x1).U)
            c.io.in.bits.prob.poke(prob.U)
            c.io.in.bits.last.poke((last && i == (n - 1) && j == 7).B)

            prob = prob + 1
            if(prob > 4095)
              prob = 1;
            c.clock.step()
          }
        }

        n = is.read(buf)
      }
      c.io.in.valid.poke(false.B)
    }.fork {
      val bd = new ByteBundle
      for((byte, i) <- outputData.zipWithIndex) {
        c.io.out.expectDequeue(bd.Lit(_.byte -> byte.U, _.last -> (i == (outputData.size - 1)).B))
      }
    }.join()

  }
}