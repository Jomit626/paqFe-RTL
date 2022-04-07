package coder

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import verifydata._
import types._

import java.io._
import com.github.tototoshi.csv._

import LocalHelpers._
import testhelpers.Helpers._

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
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "ArithCoder"

  val db = new VerifyData("./paqFe/verify/db/coder")
  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "match software model with data: " + test_name in {
      test(new ArithCoder)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      
        dutTestInit(c)
        dutTest(c, input_file, output_file)

      }
    }

    it should "tolerate throttling with data: " + test_name in {
      test(new ArithCoder)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      
        dutTestInit(c)
        dutTest(c, input_file, output_file, true)

      }
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new ArithCoder)
      .withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { c =>
      dutTestInit(c)
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        dutTest(c, input_file, output_file)
        c.clock.step(4)
      }
    }
  }
}
import scala.util.Random
private object LocalHelpers {
  def dutTestInit(c : ArithCoder) = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)
    c.io.out.initSink()
    c.io.out.setSinkClock(c.clock)

    c.reset.poke(true.B)
    c.clock.step(16)
    c.reset.poke(false.B)
    c.io.in.valid.poke(false.B)
    c.clock.step()
  }

  def dutTest(c : ArithCoder, input_file : String, output_file : String, randomThrottle: Boolean = false) = {
    val is = new ByteStream(input_file)
    val reader = CSVReader.open(new File(output_file))

    val outputData = reader.all().map(_(0).toInt)

    fork {
      val bd = new BitProbBundle()

      var prob = 2048
      var flag = true
      while(flag) {
        val (byte, last) = is.getByte()
        for(j <- 0 until 8) {
          c.io.in.enqueue(bd.Lit(
            _.bit -> ((byte >> (7 - j)) & 0x1).U,
            _.prob -> prob.U,
            _.last -> (last && j == 7).B
          ))

          prob = prob + 1
          if(prob > 4095)
            prob = 1;
        }
        
        flag = !last
      }
    }.fork {
      val bd = new ByteBundle
      for((byte, i) <- outputData.zipWithIndex) {
        c.io.out.expectDequeue(bd.Lit(_.byte -> byte.U, _.last -> (i == (outputData.size - 1)).B))
        if(randomThrottle) {
          c.clock.step(Random.between(0, 2))
        }
      }
    }.join()

  }
}