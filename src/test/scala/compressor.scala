import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.internal.VerilatorBackendAnnotation

import verifydata._
import types.ByteBundle

class CoderAribiterSpec extends FlatSpec
  with ChiselScalatestTester {
  
  behavior of "CoderAribiter"
  it should "alskdfjalsjdf" in {
    test(new CoderAribiter)
      .withAnnotations(Seq(WriteVcdAnnotation)) {c => 
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
          c.io.in(i).valid.poke(false.B)
          for(j <- 0 until 32) {
            c.io.in(i).bits.byte.poke(j.U)
            c.io.in(i).bits.last.poke((j == 31).B)
            c.io.in(i).valid.poke(true.B)
            c.clock.step()
          }
          c.io.in(i).valid.poke(false.B)
          c.clock.step()
        }
      }

      threads.join()
      for(i <- 0 until 8) 
        assert(expectData(i) == 32)
    }
  }
}
import LocalHelpers._
import java.io._
class TestCompressrorSpec extends FlatSpec
  with ChiselScalatestTester {
  behavior of "TestCompressror"

  val db = new VerifyData("../paqFe/verify/db/all")
  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should "work with " + test_name in {
      test(new TestCompressror)
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      
        init(c)
        testCompressror(c, input_file, output_file)

      } 
    }

    
  }
}

private object LocalHelpers {
  def init(c : TestCompressror) = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.out.initSink()
    c.io.out.setSinkClock(c.clock)
  }

  def testCompressror(c : TestCompressror, input_file : String, output_file : String) = {
    val is = new ByteStream(input_file)
    val oss = (0 until 8).map(i => new ByteStream(output_file + s".$i"))

    fork {
      var pack = is.getByte()
      val bd = new ByteBundle()
      while(!pack._2) {
        c.io.in.enqueue(bd.Lit(_.byte -> (pack._1 & 0xFF).U, _.last -> pack._2.B))
        pack = is.getByte()
      }
      c.io.in.enqueue(bd.Lit(_.byte -> (pack._1 & 0xFF).U, _.last -> pack._2.B))
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
        //c.io.out.bits.byte.expect((byte & 0xFF).U)

        c.clock.step()
      }
    }.join()

    for(i <- 0 until 8)
      assert(oss(i).end())
  }
}