package paqFe.models

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import OrdersHelpers._
import scala.util.Random

class OrdersTest extends Module with Model {
  val inst = Module(new Orders())
  val nProb = inst.nProb
  val nCtx = inst.nCtx

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))

    val outProb = Vec(inst.nProb, Vec(8, DecoupledIO(new BitProbBundle())))
    val outCtx = Vec(inst.nCtx, Vec(8, DecoupledIO(UInt(8.W))))

    val status = Output(new StatusBundle)
  })

  val byte2nibble = Module(new Byte2Nibble(1))

  byte2nibble.io.in <> Queue(io.in, 2)
  byte2nibble.io.out(0) <> inst.io.in

  io.outCtx <> inst.io.outCtx
  io.outProb <> inst.io.outProb

  io.status := inst.io.status
}

import chiseltest.simulator.SimulatorDebugAnnotation
class OrdersSpec extends SpecClass {
  behavior of "Orders"
  val db = new VerifyData("./verify/db/orders")
  for(line <- db.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    it should s"match software model with data: $data_name" in {
      test(new OrdersTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(input_file, output_file)
      }
    }
  
    it should s"tolerate throttling with data: $data_name" in {
      test(new OrdersTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(input_file, output_file, true)
      }
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new OrdersTest())
    .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      c.init()
      for(line <- db.data) {
        val data_name = line(0)
        val input_file = line(1)
        val output_file = line(2)
        
        c.test(input_file, output_file)
        c.waitInitDone()
      }
    }
  }
}

object OrdersHelpers {
implicit class OrdersDUT(c: OrdersTest) {
  def waitInitDone() = {
    statusWaitInitDone(c.clock,c.io.status, 1 << 17)
  }
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.outCtx.foreach{ out => out.foreach{ channel =>
      channel.initSink()
      channel.setSinkClock(c.clock)
    }}
    c.io.outProb.foreach{ out => out.foreach{ channel =>
      channel.initSink()
      channel.setSinkClock(c.clock)
    }}
    waitInitDone()
  }

  def test(input: String, output: String, throttle: Boolean = false) = {
    var processs = fork {
      val is = new ByteStream(input)
      val bd = new ByteBundle()

      var flag = true
      var cnt = 0
      while(flag) {
        val (byte, last) = is.getByte()
        c.io.in.enqueue(bd.Lit(_.byte -> (byte & 0xFF).U, _.last -> last.B))
        flag = !last
        cnt = cnt + 1
      }
    }

    new VerifyDataset(output).forAll { data =>
      for(idx <- 0 until 8) {
        for(i <- 0 until c.nProb) {
          processs = processs.fork {
            // for i th prob in a mixer
            assert(data.length % 8 == 0)
            val n = data.length / 8
            val bd = new BitProbBundle
            for(j <- 0 until n) {
              val line = data(j * 8 + idx)
              val bit = line(0)
              val p = line(1 + i)
              c.io.outProb(i)(idx).expectDequeue(bd.Lit(
                _.bit -> bit.U,
                _.prob -> p.U,
                _.last -> (j == n-1).B
              ))
              
              if(throttle) {
                c.clock.step(Random.between(0, 2))
              }
            }
          }
        }

        for(i <- 0 until c.nCtx) {

          processs = processs.fork { // mixer ctx
            val n = data.length / 8
            for(j <- 0 until n) {
              val line = data(j * 8 + idx)
              val ctx = line(1 + c.nProb + i)
              c.io.outCtx(i)(idx).expectDequeue(ctx.U)

              if(throttle)
                c.clock.step(Random.between(0, 2))
            }
          }
        }
      }
      processs.join()
    }
  }
}
}
