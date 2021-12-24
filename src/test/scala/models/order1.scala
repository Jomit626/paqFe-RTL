package models

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

class Order1TestWapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))
    val out = Vec(8, ValidIO(new BitProbBundle()))

    val status = new StatusBundle
  })

  val byte2nibble = Module(new Byte2Nibble(1))
  val order1 = Module(new Order1())

  byte2nibble.io.in <> io.in
  order1.io.in <> byte2nibble.io.out(0)

  (0 until 8).map{ i =>
    io.out(i) := order1.io.out(i)
  }

  io.status := order1.io.status
}

class Order1Spec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "Order1 and Byte2Nibble"

  val db = new VerifyData("../paqFe/verify/db/order1-db")
  for(line <- db.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "match software model with data: " + data_name in {
      test(new Order1TestWapper)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      
        dutTestInit(c)
        statusWaitInitDone(c.clock, c.io.status)

        dutTest(c, input_file, output_file)
      }
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new Order1TestWapper)
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      dutTestInit(c)
      for(line <- db.data) {
        val data_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        statusWaitInitDone(c.clock, c.io.status)
        dutTest(c, input_file, output_file)
      }
    }
  }
  
}


private object LocalHelpers {
  def dutTestInit(c : Order1TestWapper) = {
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
  }

  def dutTest(c : Order1TestWapper, input_file : String, output_file : String) = {
    val is = new ByteStream(input_file)
    val reader = CSVReader.open(new File(output_file))

    val bitProbs = reader.all().map{line => (line(0).toInt, line(1).toInt)}.toIndexedSeq
    assert(bitProbs.length % 8 == 0)

    var thr = fork {
      val bd = new ByteBundle()
      var flag = true
      while(flag) {
        val (byte, last) = is.getByte()
        c.io.in.enqueue(bd.Lit(_.byte -> (byte & 0xFF).U, _.last -> last.B))
        
        flag = !last
      }
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