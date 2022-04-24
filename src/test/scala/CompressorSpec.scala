package paqFe

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import verifydata._
import types._

import LocalHelpers._

class CoderAribiterSpec extends SpecClass {
  behavior of "CoderAribiter"
  it should "merge 8 stream into 1 tagged with idx and set last singal currectly" in {
    test(new CoderAribiter) {c => 
      for(i <- 0 until 8) {
        c.io.in(i).initSource()
        c.io.in(i).setSourceClock(c.clock)
      }

      c.io.out.initSink()
      c.io.out.setSinkClock(c.clock)


      val expectData = new Array[Byte](8)
      for(i <- 0 until 8) 
        expectData(i) = 0
      
      var threads = fork {
        var last = false
        c.io.out.ready.poke(true.B)
        while(!last) {
          var valid = c.io.out.valid.peek().litToBoolean
          while(!valid) {
            c.clock.step()
            valid = c.io.out.valid.peek().litToBoolean
          }

          val idx = c.io.out.bits.idx.peek().litValue.toInt
          last = c.io.out.bits.last.peek().litToBoolean
          c.io.out.bits.byte.expect(expectData(idx).U)

          expectData(idx) = ((expectData(idx) + 1) & 0xFF).toByte
          c.clock.step()
        }
      }

      for(i <- 0 until 8) {
        threads = threads.fork {
          val bd = new ByteBundle
          for(j <- 0 until 32) {
            c.io.in(i).enqueue(bd.Lit(
              _.byte -> j.U,
              _.last -> (j == 31).B
            ))
          }
        }
      }

      threads.join()
      for(i <- 0 until 8) 
        assert(expectData(i) == 32)
    }
  }
}

class CompressrorSpec extends SpecClass {
  behavior of "Compressror"

  val db = new VerifyData("./verify/db/all")
  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "match software model with data: " + test_name in {
      test(new CompressorTest)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        c.init()
        statusWaitInitDone(c.clock, c.io.status, 1 << 17)
        c.test(input_file, output_file)
      } 
    }

    it should "tolerate throttling with data: " + test_name in {
      test(new CompressorTest)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        c.init()
        statusWaitInitDone(c.clock, c.io.status, 1 << 18)
        c.test(input_file, output_file)
      } 
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new CompressorTest)
    .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      c.init()
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        statusWaitInitDone(c.clock, c.io.status, 1 << 18)
        c.test(input_file, output_file)
      }
    } 
  }
}

class CompressorTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))
    val out = DecoupledIO(new ByteIdxBundle())

    val status = new StatusBundle
  })

  val inst = Module(new Compressor())
  inst.coder_clk := clock
  inst.coder_rst := reset
  inst.model_clk := clock
  inst.model_rst := reset
  inst.mixer_clk := clock
  inst.mixer_rst := reset

  io.in <> inst.model_in
  inst.coder_out <> io.out

  io.status := inst.model_status
}

private object LocalHelpers {
implicit class CompressorTestDUT(c: CompressorTest) {
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.out.initSink()
    c.io.out.setSinkClock(c.clock)
  }

  def test(input_file : String, output_file : String, randomThrottle: Boolean = false) = {
    val is = new ByteStream(input_file)
    val oss = (0 until 8).map(i => new ByteStream(output_file + s".$i"))

    fork {
      val bd = new ByteBundle()
      var flag = true
      while(flag) {
        val (byte, last) = is.getByte()
        c.io.in.enqueue(bd.Lit(_.byte -> (byte & 0xFF).U, _.last -> last.B))
        
        flag = !last
      }
    }.fork {
      var last = false
      c.io.out.ready.poke(true.B)
      while(!last) {
        var valid = c.io.out.valid.peek().litToBoolean
        while(!valid) {
          c.clock.step()
          valid = c.io.out.valid.peek().litToBoolean
        }

        val idx = c.io.out.bits.idx.peek().litValue.toInt
        val (byte, streamEnd) = oss(idx).getByte()

        last = c.io.out.bits.last.peek().litToBoolean
        c.io.out.bits.byte.expect((byte & 0xFF).U)

        c.clock.step()
        if(randomThrottle) {
          c.io.out.ready.poke(false.B)
          val t = Random.between(0, 128)
          c.clock.step((t - 32) max 0)
          c.io.out.ready.poke(true.B)
        }
      }
    }.join()

    for(i <- 0 until 8)
      assert(oss(i).end())
  }
}
}
