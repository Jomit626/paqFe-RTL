package models

import chisel3._
import chisel3.util._

import ram._
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
  val valid = io.in.valid

  val nibble_d = RegNext(nibble)
  val context_d = RegNext(context)
  val valid_d = RegNext(valid, false.B)

  val nibble_dd = RegNext(nibble_d)
  val context_dd = RegNext(context_d)
  val valid_dd = RegNext(valid_d, false.B)

  val harzared1 = context_d === context_dd && valid_d && valid_dd
  val harzared2 = context === context_dd && valid && valid_dd
  val harzared2_d = RegNext(harzared2, false.B)

  // duel port, A write first, B read only
  val ram = Module(new ByteWriteTDPRam(16, CtxWidth))

  ram.io.enb := io.in.valid
  ram.io.addrb := context

  val dout = Reg(UInt((8 * 16).W))
  
  val line_split = (0 until 16).map(i => dout(8*i+7,8*i))
  
  val checksum = line_split(0)
  val checksumNxt = context_dd(7,0)
  val hit = checksum === checksumNxt

  val line = (0 until 16).map(i => Mux(hit, dout(8*i+7,8*i), 0.U))

  val state0 = line(1)
  val state0Nxt = Seq(StateShiftLut(state0, nibble_dd(3)))

  val state1group = (2 until 4).map(line(_))
  val state1OH = UIntToOH(nibble_dd(3))
  val state1 = Mux1H(state1OH, state1group)
  val state1Nxt = state1group.map(StateShiftLut(_, nibble_dd(2))).zipWithIndex.map{case (s,i) => Mux(state1OH(i), s, state1group(i))}.reverse

  val state2group = (4 until 8).map(line(_))
  val state2OH = UIntToOH(nibble_dd(3,2))
  val state2 = Mux1H(state2OH, state2group)
  val state2Nxt = state2group.map(StateShiftLut(_, nibble_dd(1))).zipWithIndex.map{case (s,i) => Mux(state2OH(i), s, state2group(i))}.reverse

  val state3group = (8 until 16).map(line(_))
  val state3OH = UIntToOH(nibble_dd(3,1))
  val state3 = Mux1H(state3OH, state3group)
  val state3Nxt = state3group.map(StateShiftLut(_, nibble_dd(0))).zipWithIndex.map{case (s,i) => Mux(state3OH(i), s, state3group(i))}.reverse

  val lineNxt = Cat(state3Nxt ++ state2Nxt ++ state1Nxt ++ state0Nxt ++ Seq(checksumNxt))
  val wen = Cat(Seq(state3OH,state2OH,state1OH,1.U(1.W),1.U(1.W)))

  ram.io.ena := valid_dd
  ram.io.wea := wen
  ram.io.addra := context_dd
  ram.io.dina := lineNxt

  val outSel = RegInit(false.B)
  when(valid_dd) {
    outSel := ~outSel
  }

  when(harzared1) {
    dout := lineNxt
  }.elsewhen(harzared2_d) {
    dout := ram.io.doa
  }.otherwise{
    dout := ram.io.dob
  }

  val first = RegEnable(false.B, true.B, io.out(0).valid)
  val probs = Seq(state0, state1, state2, state3).map(StaticStateMap(_))

  for(i <- 0 until 4) {
    if(i == 0)
      io.out(i).bits.prob := Mux(first, 2048.U, probs(i))
    else 
      io.out(i).bits.prob := probs(i)
    io.out(i).bits.bit := nibble_dd(3 - i)
    io.out(i).valid := valid_dd && ~outSel
    
    io.out(i + 4).bits.prob := probs(i)
    io.out(i + 4).bits.bit := nibble_dd(3 - i)
    io.out(i + 4).valid := valid_dd && outSel
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