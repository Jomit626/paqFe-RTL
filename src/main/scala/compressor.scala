import chisel3._
import chisel3.util._

import types._
import ram._
import coder.ArithCoder
import models.{Order1, Byte2Nibble}

class CoderAribiter extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new ByteBundle())))
    val out = DecoupledIO(new ByteIdxBundle())
  })

  val queues = Seq.fill(8)(Module(new Queue(new ByteBundle(), 32)))
  val arb = Module(new RRArbiter(new ByteBundle(), 8))

  (0 until 8).map{ i =>
    queues(i).io.enq <> io.in(i)

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
  def apply(ins : Vec[DecoupledIO[BitProbBundle]],
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

      queues(i).io.enq <> ins(i)

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
    coders(i).io.in  <> order1.io.out(i)
    arib.io.in(i) <> coders(i).io.out
  }

  io.out <> arib.io.out

  io.status := order1.io.status
}

class CompressrorNoCDCWrapped extends RawModule {
  val ACLK = IO(Input(Clock()))
  val ARESTN = IO(Input(Bool()))
  val S_AXIS = IO(new Bundle {
    val TDATA = Input(UInt(8.W))
    val TKEEP = Input(UInt(1.W))
    val TLAST = Input(Bool())
    val TREADY = Output(Bool())
    val TVALID = Input(Bool())
  })

  val M_AXIS = IO(new Bundle {
    val TDATA = Output(UInt(16.W))
    val TKEEP = Output(UInt(2.W))
    val TLAST = Output(Bool())
    val TREADY = Input(Bool())
    val TVALID = Output(Bool())
  })
  withClockAndReset(ACLK, ~ARESTN) {
    val packetOutput = Module(new PacketOutput)
    val inst = Module(new CompressrorNoCDC)
    
    inst.io.in.bits.byte := S_AXIS.TDATA
    inst.io.in.bits.last := S_AXIS.TLAST
    inst.io.in.valid := S_AXIS.TVALID & inst.io.status.initDone
    S_AXIS.TREADY := inst.io.in.ready & inst.io.status.initDone

    packetOutput.io.in.bits := inst.io.out.bits
    packetOutput.io.in.valid := inst.io.out.valid
    inst.io.out.ready := packetOutput.io.in.ready

    M_AXIS.TDATA := packetOutput.io.out.bits.data
    M_AXIS.TLAST := packetOutput.io.out.bits.last
    M_AXIS.TKEEP := "b11".U
    M_AXIS.TVALID := packetOutput.io.out.valid
    packetOutput.io.out.ready := M_AXIS.TREADY
  }
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

class CompressrorWrapped extends RawModule {
  val S_AXIS_ACLK = IO(Input(Clock()))
  val S_AXIS_ARESTN = IO(Input(Bool()))

  val M_AXIS_ACLK = IO(Input(Clock()))
  val M_AXIS_ARESTN = IO(Input(Bool()))

  val S_AXIS = IO(new Bundle {
    val TDATA = Input(UInt(8.W))
    val TKEEP = Input(UInt(1.W))
    val TLAST = Input(Bool())
    val TREADY = Output(Bool())
    val TVALID = Input(Bool())
  })

  val M_AXIS = IO(new Bundle {
    val TDATA = Output(UInt(16.W))
    val TKEEP = Output(UInt(2.W))
    val TLAST = Output(Bool())
    val TREADY = Input(Bool())
    val TVALID = Output(Bool())
  })
  val inst = Module(new Compressor)
  
  inst.model_clk := S_AXIS_ACLK
  inst.model_rst := ~S_AXIS_ARESTN
  inst.model_in.bits.byte := S_AXIS.TDATA
  inst.model_in.bits.last := S_AXIS.TLAST
  inst.model_in.valid := S_AXIS.TVALID & inst.model_status.initDone
  S_AXIS.TREADY := inst.model_in.ready & inst.model_status.initDone

  inst.coder_clk := M_AXIS_ACLK
  inst.coder_rst := ~M_AXIS_ARESTN
  M_AXIS.TDATA := Cat(inst.coder_out.bits.idx, inst.coder_out.bits.byte)
  M_AXIS.TLAST := inst.coder_out.bits.last
  M_AXIS.TKEEP := 3.U
  M_AXIS.TVALID := inst.coder_out.valid
  inst.coder_out.ready := M_AXIS.TREADY
}

import chisel3.stage.ChiselStage
object GetCompressorVerilog extends App {
  (new ChiselStage).emitVerilog(new CompressrorNoCDCWrapped)
}
