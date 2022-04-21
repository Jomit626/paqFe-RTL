package paqFe

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.coder.ArithCoder
import paqFe.mixer._
import paqFe.models._
import paqFe.util._

class Model2MixerCrossing(implicit p: MixerParameter) extends RawModule {
  val io = IO(new Bundle {
    val model_clk = Input(Clock())
    val model_rst = Input(Bool())
    val in = Vec(8, Flipped(DecoupledIO(new MixerInputBundle)))

    val mixer_clk = Input(Clock())
    val mixer_rst = Input(Bool())
    val out = Vec(8, DecoupledIO(new MixerInputBundle))
  })

  for(i <- 0 until 8) {
    val asyncQueue = Module(new AsyncQueue(chiselTypeOf(io.in(i).bits), 8))

    asyncQueue.io.enq_clock := io.model_clk
    asyncQueue.io.enq_reset := io.model_rst
    io.in(i) <> asyncQueue.io.enq

    asyncQueue.io.deq_clock := io.mixer_clk
    asyncQueue.io.deq_reset := io.mixer_rst
    withClockAndReset(io.mixer_clk, io.mixer_rst) {
      DecoupledSkidBuf(DecoupledHalfRegSlice(asyncQueue.io.deq)) <> io.out(i)
    }
  }
}

class Mixer2CoderCrossing extends RawModule {
  val io = IO(new Bundle {
    val mixer_clk = Input(Clock())
    val mixer_rst = Input(Bool())
    val in = Vec(8, Flipped(DecoupledIO(new BitProbBundle)))

    val coder_clk = Input(Clock())
    val coder_rst = Input(Bool())
    val out = Vec(8, DecoupledIO(new BitProbBundle))
  })

  for(i <- 0 until 8) {
    val asyncQueue = Module(new AsyncQueue(chiselTypeOf(io.in(i).bits), 8))

    asyncQueue.io.enq_clock := io.mixer_clk
    asyncQueue.io.enq_reset := io.mixer_rst
    io.in(i) <> asyncQueue.io.enq

    asyncQueue.io.deq_clock := io.coder_clk
    asyncQueue.io.deq_reset := io.coder_rst
    withClockAndReset(io.coder_clk, io.coder_rst) {
      DecoupledSkidBuf(DecoupledHalfRegSlice((asyncQueue.io.deq))) <> io.out(i)
    }
  }
}
