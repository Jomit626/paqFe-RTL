package paqFe

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.coder.ArithCoder
import paqFe.mixer._
import paqFe.models._
import paqFe.util._

class CoderAribiter extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new ByteBundle())))
    val out = DecoupledIO(new ByteIdxBundle())
  })

  val queues = Seq.fill(8)(Module(new Queue(new ByteBundle(), 8)))
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
  io.out.bits.idx := arb.io.chosen  // TODO: batter encoding
  io.out.bits.last := PopCount(lasts) === 7.U && arb.io.out.bits.last

  io.out.valid := arb.io.out.valid
  arb.io.out.ready := io.out.ready
  
}

object Mixer2CoderCrossing {
  def apply(ins : Vec[DecoupledIO[BitProbBundle]],
            mClock : Clock, mRest : Bool,
            cClock : Clock, cRest : Bool)
     : Vec[DecoupledIO[BitProbBundle]] = {
    val outs = Wire(Vec(ins.length, new DecoupledIO(new BitProbBundle)))
    val queues = Seq.fill(ins.length) {Module(new AsyncQueue(new BitProbBundle, 8))}

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

object Model2MixerCrossing {
  def apply(in: Vec[DecoupledIO[MixerInputBundle]],
            modelClock: Clock, modelRest: Bool,
            mixerClock: Clock, mixerRest: Bool) = {
    val out = Wire(Vec(in.length, DecoupledIO(chiselTypeOf(in.head.bits))))
    val queues = Seq.fill(in.length) {Module(new AsyncQueue(chiselTypeOf(in.head.bits), 8))}

    (0 until in.length).map{ i =>
      queues(i).io.enq_clock := modelClock
      queues(i).io.enq_reset := modelRest
      queues(i).io.deq_clock := mixerClock
      queues(i).io.deq_reset := mixerRest

      queues(i).io.enq <> in(i)

      out(i) <> queues(i).io.deq
    }
    
    out
  }
}

class Compressor extends RawModule {
  val model_clk = IO(Input(Clock()))
  val model_rst = IO(Input(Bool()))
  
  val model_in = IO(Flipped(DecoupledIO(new ByteBundle())))
  val model_status = IO(new StatusBundle)
  
  val mixer_clk = IO(Input(Clock()))
  val mixer_rst = IO(Input(Bool()))
  
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

  implicit val p = new MixerParameter(4, 1)
  var orders: Orders = null
  var mixers: Seq[Mixer] = null
  var gatterScatter: ProbGatterScatter = null

  
  withClockAndReset(model_clk, model_rst) {
    val byte2nibble = Module(new Byte2Nibble(1))
    gatterScatter = Module(new ProbGatterScatter())
    orders = Module(new Orders())
    
    byte2nibble.io.in <> model_in
    orders.io.in <> byte2nibble.io.out(0)

    gatterScatter.io.in <> orders.io.outProb
    gatterScatter.io.inCtx <> VecInit(Seq(orders.io.outCtx))

    model_status := orders.io.status
  }

  withClockAndReset(mixer_clk, mixer_rst) {
    mixers = Seq.fill(8){Module(new Mixer)}
    val crossed = Model2MixerCrossing(gatterScatter.io.out, model_clk, model_rst, mixer_clk, mixer_rst)

    for(mixerIdx <- 0 until 8) {
      mixers(mixerIdx).io.in <> crossed(mixerIdx)
    }
  }

  withClockAndReset(coder_clk, coder_rst) {
    val modelOut = Mixer2CoderCrossing(VecInit(mixers.map(_.io.out)), model_clk, model_rst, coder_clk, coder_rst)
    
    (0 until 8).map{ i =>
      DecoupledHalfRegSlice(modelOut(i)) <> coders(i).io.in
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
  var packetOutput: PacketOutput = null
  withClockAndReset(M_AXIS_ACLK, ~M_AXIS_ARESTN) {
    packetOutput = Module(new PacketOutput)
  }
  val inst = Module(new Compressor)

  
  inst.model_clk := S_AXIS_ACLK
  inst.model_rst := ~S_AXIS_ARESTN
  inst.model_in.bits.byte := S_AXIS.TDATA
  inst.model_in.bits.last := S_AXIS.TLAST
  inst.model_in.valid := S_AXIS.TVALID & inst.model_status.initDone
  S_AXIS.TREADY := inst.model_in.ready & inst.model_status.initDone

  inst.coder_clk := M_AXIS_ACLK
  inst.coder_rst := ~M_AXIS_ARESTN
  
  packetOutput.io.in <> inst.coder_out

  M_AXIS.TDATA := packetOutput.io.out.bits.data
  M_AXIS.TLAST := packetOutput.io.out.bits.last
  M_AXIS.TKEEP := "b11".U
  M_AXIS.TVALID := packetOutput.io.out.valid
  packetOutput.io.out.ready := M_AXIS.TREADY
}

import chisel3.stage.ChiselStage
object GetCompressorVerilog extends App {
  (new ChiselStage).emitVerilog(new Compressor)
}

import models.ContextMap
import paqFe.mixer.Mixer
import paqFe.models._
import paqFe.util._
object GetTestVerilog extends App {
  (new ChiselStage).emitVerilog(new Compressor)
}
