import chisel3._
import chisel3.util._

import types._
import ram._
import coder.ArithCoder
import models.{Order1, Byte2Nibble}

class CoderAribiter extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(ValidIO(new ByteBundle())))
    val out = DecoupledIO(new ByteIdxBundle())
  })

  val queues = Seq.fill(8)(Module(new Queue(new ByteBundle(), 32)))
  val arb = Module(new RRArbiter(new ByteBundle(), 8))

  (0 until 8).map{ i =>
    queues(i).io.enq.bits := io.in(i).bits
    queues(i).io.enq.valid := io.in(i).valid

    arb.io.in(i) <> queues(i).io.deq
  }
  
  val init = io.out.fire && io.out.bits.last
  val lasts = Seq.fill(8) {RegInit(false.B)}
  (0 until 8).map{ i =>
    lasts(i) := (lasts(i) | (queues(i).io.deq.fire && queues(i).io.deq.bits.last)) & ~init
  }

  io.out.bits.byte := arb.io.out.bits.byte
  io.out.bits.idx := arb.io.chosen
  io.out.bits.last := PopCount(lasts) === 7.U && arb.io.out.bits.last

  io.out.valid := arb.io.out.valid
  arb.io.out.ready := io.out.ready
  
}

object Model2CoderCrossing {
  def apply(ins : Vec[ValidIO[BitProbBundle]],
            mClock : Clock, mRest : Bool,
            cClock : Clock, cRest : Bool)
     : Vec[DecoupledIO[BitProbBundle]] = {
    val outs = Wire(Vec(ins.length, new DecoupledIO(new BitProbBundle)))
    val queues = Seq.fill(ins.length) {Module(new AsyncQueue(new BitProbBundle, 32))}

    (0 until ins.length).map{ i =>
      queues(i).io.enq_clock := mClock
      queues(i).io.enq_reset := mRest
      queues(i).io.deq_clock := cClock
      queues(i).io.deq_reset := cRest

      queues(i).io.enq.bits := ins(i).bits
      queues(i).io.enq.valid := ins(i).valid

      outs(i) <> queues(i).io.deq
    }
    
    outs
  }
}

class CompressrorNoCDC extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))
    val out = DecoupledIO(new ByteIdxBundle())

    val status = new StatusBundle
  })

  val byte2nibble = Module(new Byte2Nibble(1))
  val order1 = Module(new Order1())
  val coders = Seq.fill(8) {Module(new ArithCoder())}
  val arib = Module(new CoderAribiter)

  byte2nibble.io.in <> io.in

  order1.io.in <> byte2nibble.io.out(0)

  (0 until 8).map{ i =>
    coders(i).io.in  <> Pipe(order1.io.out(i))
    arib.io.in(i) <> coders(i).io.out
  }

  io.out <> arib.io.out

  io.status := order1.io.status
}

class Compressor extends RawModule {
  val model_clk = IO(Input(Clock()))
  val model_rst = IO(Input(Bool()))

  val model_in = IO(Flipped(DecoupledIO(new ByteBundle())))
  val model_status = IO(new StatusBundle)

  val coder_clk = IO(Input(Clock()))
  val coder_rst = IO(Input(Bool()))
  
  val coder_out = IO(DecoupledIO(new ByteIdxBundle()))

  var coders : Seq[ArithCoder] = null

  withClockAndReset(coder_clk, coder_rst) {
    coders = Seq.fill(8) {Module(new ArithCoder())}
    val arib = Module(new CoderAribiter)

    (0 until 8).map{ i =>
      arib.io.in(i) <> coders(i).io.out
    }
    coder_out <> arib.io.out
  }

  var order : Order1 = null

  withClockAndReset(model_clk, model_rst) {
    val byte2nibble = Module(new Byte2Nibble(1))
    order = Module(new Order1())

    byte2nibble.io.in <> model_in
    order.io.in <> byte2nibble.io.out(0)

    model_status := order.io.status
  }

  withClockAndReset(coder_clk, coder_rst) {
    val modelOut = Model2CoderCrossing(order.io.out, model_clk, model_rst, coder_clk, coder_rst)
    
    (0 until 8).map{ i =>
      coders(i).io.in.bits := RegNext(modelOut(i).bits)
      coders(i).io.in.valid := RegNext(modelOut(i).valid, false.B)
      modelOut(i).ready := true.B
    }
  }
}

import chisel3.stage.ChiselStage
object GetCompressorVerilog extends App {
  (new ChiselStage).emitVerilog(new Compressor)
}

import  models.ContextMap
object GetTestVerilog extends App {
  (new ChiselStage).emitVerilog(new ContextMap(15))
}
