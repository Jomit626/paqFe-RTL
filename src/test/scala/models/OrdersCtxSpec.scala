package paqFe.models

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import OrderCtxHelpers._
import scala.util.Random

class OrdersContextTest extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))

    val o1Out = DecoupledIO(new NibbleCtxBundle(12))
    val o2Out = DecoupledIO(new NibbleCtxBundle(16))
    val o3Out = DecoupledIO(new NibbleCtxBundle(16))
    val o4Out = DecoupledIO(new NibbleCtxBundle(17))
  })

  val byte2nibble = Module(new Byte2Nibble(1))
  val contextGen = Module(new OrdersContext())

  byte2nibble.io.in <> Queue(io.in, 16)
  contextGen.io.in <> byte2nibble.io.out(0)

  io.o1Out <> contextGen.io.o1Out
  io.o2Out <> contextGen.io.o2Out
  io.o3Out <> contextGen.io.o3Out
  io.o4Out <> contextGen.io.o4Out
}

class OrdersCtxSpec extends SpecClass {
  behavior of "OrdersCtx"
  val db = new VerifyData("./paqFe/verify/db/ordersctx")
  for(line <- db.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    it should s"match software model with data: $data_name" in {
      test(new OrdersContextTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(input_file, output_file)
      }
    }

    it should s"tolerate throttling with data: $data_name" in {
      test(new OrdersContextTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(input_file, output_file, true)
      }
    }
  }
  
  it should "take multiple streams without reset between and output currect data" in {
    test(new OrdersContextTest())
    .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      c.init()
      for(line <- db.data) {
        val data_name = line(0)
        val input_file = line(1)
        val output_file = line(2)
        
        c.test(input_file, output_file)
      }
    }
  }
}

object OrderCtxHelpers {

implicit class OrdersCtxDUT(c: OrdersContextTest) {
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    Seq(c.io.o1Out, c.io.o2Out, c.io.o3Out, c.io.o4Out).foreach { out =>
      out.initSink()
      out.setSinkClock(c.clock)
    }
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
      processs = processs.fork {
        expectOut(data, c.io.o1Out, 1, throttle, 12)
      }.fork {
        expectOut(data, c.io.o2Out, 2, throttle, 16)
      }.fork {
        expectOut(data, c.io.o3Out, 3, throttle, 16)
      }.fork {
        expectOut(data, c.io.o4Out, 4, throttle, 17)
      }
    }

    processs.join()
  }

  private def expectOut(data: IndexedSeq[Seq[Int]], out: DecoupledIO[NibbleCtxBundle], idx: Int, throttle: Boolean, width: Int) = {
    val bd = new NibbleCtxBundle(width)
    for((line, i) <- data.zipWithIndex) {
      val nibble = line(0)
      val ctx = line(idx)
      val chk = line(idx + 4)
      out.expectDequeue(bd.Lit(
        _.context -> ctx.U,
        _.nibble -> nibble.U,
        _.chk -> chk.U,
        _.last -> (i >= data.length - 2).B
      ))
      if(throttle) {
        c.clock.step(Random.between(0, 4))
      }
    }
  }
}

}
