package models

import chisel3._
import chisel3.util._

import ram.ByteWriteTDPRamFWB
import state.StateShiftLut
import state.StaticStateMap
import coder.BitProbBundle

class Byte2Nibble(n : Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))
    val out = Vec(n, ValidIO(new NibbleBundle()))
  })

  val s0 = 0.U(1.W)
  val s1 = 1.U(1.W)
  val stateNxt = Wire(UInt())
  val state = RegNext(stateNxt, s0)

  stateNxt := Mux(io.in.valid, ~state, state)

  io.in.ready := state === s1

  for (i <- 0 until n) {
    io.out(i).bits.nibble := RegNext(Mux(state === s1, io.in.bits.byte(7,4), io.in.bits.byte(3,0)))
    io.out(i).valid := RegNext(io.in.valid, 0.U)
  }
}


class ByteBundle extends Bundle {
  val byte = Output(UInt(8.W))
}

class NibbleBundle extends Bundle {
  val nibble = Output(UInt(4.W))
}

class CtxBundle(CtxWidth : Int) extends Bundle {
  val context = Output(UInt(CtxWidth.W))
}

class NibbleCtxBundle(CtxWidth : Int) extends Bundle {
  val nibble = Output(UInt(4.W))
  val context = Output(UInt(CtxWidth.W))
}

class ContextMap(CtxWidth : Int) extends Module {

  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleCtxBundle(CtxWidth)))
    val out = Vec(8, ValidIO(new BitProbBundle()))
  })

  val nibble = io.in.bits.nibble
  val context = io.in.bits.context

  val nibble_d = RegNext(nibble)
  val context_d = RegNext(context)
  val valid_d = RegNext(io.in.valid, false.B)

  val nibble_dd = RegNext(nibble_d)
  val context_dd = RegNext(context_d)
  val valid_dd = RegNext(valid_d, false.B)

  val ram = Module(new ByteWriteTDPRamFWB(16, CtxWidth))

  ram.io.enb := io.in.valid
  ram.io.addrb := context

  val dout = ram.io.dob

  val line = (0 until 16).map(i => dout(8*i+7,8*i))
  
  val checksum = line(0)
  val checksumNxt = context_d(7,0)
  val hit = checksum === checksumNxt

  val state0sel = line(1)
  val state0 = Mux(hit, state0sel, 0.U)
  val state0Nxt = StateShiftLut(state0, nibble_d(3))

  val state1group = (2 until 4).map(line(_))
  val state1OH = UIntToOH(nibble(3))
  val state1sel = Mux1H(state1OH, state1group)
  val state1 = Mux(hit, state1sel, 0.U)
  val state1Nxt = StateShiftLut(state1, nibble_d(2))

  val state2group = (4 until 8).map(line(_))
  val state2OH = UIntToOH(nibble(3,2))
  val state2sel = Mux1H(state2OH, state2group)
  val state2 = Mux(hit, state2sel, 0.U)
  val state2Nxt = StateShiftLut(state2, nibble_d(1))

  val state3group = (8 until 16).map(line(_))
  val state3OH = UIntToOH(nibble(3,1))
  val state3sel = Mux1H(state3OH, state3group)
  val state3 = Mux(hit, state3sel, 0.U)
  val state3Nxt = StateShiftLut(state3, nibble_d(0))

  val lineNxt = Cat(Seq(
    state3Nxt,state3Nxt,state3Nxt,state3Nxt,state3Nxt,state3Nxt,state3Nxt,state3Nxt,
    state2Nxt,state2Nxt,state2Nxt,state2Nxt,
    state1Nxt,state1Nxt,
    state0Nxt,
    checksumNxt
  ))
  val wen = Cat(Seq(state3OH,state2OH,state1OH,1.U(1.W),1.U(1.W)))

  ram.io.ena := valid_d
  ram.io.wea := wen
  ram.io.addra := context_d
  ram.io.dina := lineNxt

  val outSel = RegInit(false.B)
  when(valid_d) {
    outSel := ~outSel
  }

  val probs = Seq(state0, state1, state2, state3).map(StaticStateMap(_))
  for(i <- 0 until 4) {
    io.out(i).bits.prob := probs(i)
    io.out(i).bits.bit := nibble_d(3 - i)
    io.out(i).valid := valid_d && ~outSel

    io.out(i + 4).bits.prob := probs(i)
    io.out(i + 4).bits.bit := nibble_d(3 - i)
    io.out(i + 4).valid := valid_d && outSel
  }

}

class Order1Context extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleBundle()))
    val out = ValidIO(new NibbleCtxBundle(12))
  })

  val ctxNxt = Wire(UInt())
  val ctx = RegEnable(ctxNxt, 0.U, io.in.valid)

  ctxNxt := Cat(ctx(7, 0), io.in.bits.nibble)

  io.out.bits.context := ctx
  io.out.bits.nibble := io.in.bits.nibble
  io.out.valid := io.in.valid
}


class Order1 extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleBundle()))
    val out = Vec(8, ValidIO(new BitProbBundle()))
  })

  val contextGen = Module(new Order1Context()).io
  val contextMap = Module(new ContextMap(12)).io

  contextGen.in := io.in
  contextMap.in := contextGen.out
  (0 until 8).map(i => io.out(i) := contextMap.out(i))
}