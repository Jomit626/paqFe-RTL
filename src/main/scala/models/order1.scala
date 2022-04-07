package models

import chisel3._
import chisel3.util._

import ram._
import state.StateShiftLut
import state.StaticStateMap

import types._

class Byte2Nibble(n : Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteBundle()))
    val out = Vec(n, DecoupledIO(new NibbleBundle()))
  })

  val inReady = WireInit(false.B)
  val nibble = WireInit(0.U(4.W))
  val last = WireInit(false.B)
  val outValid = WireInit(false.B)

  val s0 :: s1 :: Nil = Enum(2)
  val state = RegInit(s0)

  val allReady = io.out.map(_.ready).reduce(_ && _)
  
  switch(state) {
    is(s0) {
      nibble := io.in.bits.byte(7,4)
      last := io.in.bits.last
      outValid := allReady && io.in.valid

      when(allReady && io.in.valid) {
        state := s1
      }
    }

    is(s1) {
      nibble := io.in.bits.byte(3,0)
      last := io.in.bits.last

      inReady := allReady
      outValid := allReady

      when(allReady && io.in.valid) {
        state := s0
      }
    }
  }

  io.in.ready := inReady

  for (i <- 0 until n) {
    io.out(i).bits.nibble := nibble
    io.out(i).bits.last := last
    io.out(i).valid := outValid
  }
}

class RamInitUnit(AddrWidth : Int) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(Bool()))

    val wen = Output(Bool())
    val waddr = Output(UInt(AddrWidth.W))

    val status = new StatusBundle
  })

  val initDone = RegInit(false.B)
  val (cnt, cntDone) = Counter(0 until (1 << AddrWidth), ~initDone)

  initDone := (initDone || cntDone) && ~(io.in.valid && io.in.bits)

  io.wen := ~initDone
  io.waddr := cnt

  io.status.initDone := initDone
}

class ContextMap(CtxWidth : Int) extends Module {

  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleCtxBundle(CtxWidth)))
    val out = Vec(8, DecoupledIO(new BitProbBundle()))

    val status = new StatusBundle
  })

  val pipelineReady = Wire(Bool())

  val nibble = io.in.bits.nibble
  val context = io.in.bits.context
  val last = io.in.bits.last
  val valid = io.in.fire

  val nibble_d = RegEnable(nibble, pipelineReady)
  val context_d = RegEnable(context, pipelineReady)
  val last_d = RegEnable(last, false.B, pipelineReady)
  val valid_d = RegEnable(valid, false.B, pipelineReady)

  val nibble_dd = RegEnable(nibble_d, pipelineReady)
  val context_dd = RegEnable(context_d, pipelineReady)
  val last_dd = RegEnable(last_d, false.B, pipelineReady)
  val valid_dd = RegEnable(valid_d, false.B, pipelineReady)

  val harzared1 = context_d === context_dd && valid_d && valid_dd
  val harzared2 = context === context_dd && valid && valid_dd
  val harzared2_d = RegEnable(harzared2, false.B, pipelineReady)

  // duel port, A write first, B read only
  val ram = Module(new ByteWriteTDPRam(16, CtxWidth))
  val ramInit = Module(new RamInitUnit(CtxWidth))

  ram.io.enb := io.in.valid && pipelineReady
  ram.io.addrb := context

  ramInit.io.in.bits := last_dd & last_d
  ramInit.io.in.valid := valid_dd && pipelineReady

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

  ram.io.ena := (valid_dd & pipelineReady) | ramInit.io.wen
  ram.io.wea := wen | Cat(Seq.fill(wen.getWidth){ramInit.io.wen})
  ram.io.addra := Mux(ramInit.io.status.initDone, context_dd, ramInit.io.waddr)
  ram.io.dina := Mux(ramInit.io.status.initDone, lineNxt, 0.U)
  
  val outSel = RegInit(false.B)
  when(valid_dd && pipelineReady) {
    outSel := ~outSel
  }
  pipelineReady := Mux(outSel, io.out.slice(4,8).map(_.ready).reduce(_ && _), io.out.slice(0,4).map(_.ready).reduce(_ && _))

  when(pipelineReady) {
    when(harzared1) {
      dout := lineNxt
    }.elsewhen(harzared2_d) {
      dout := ram.io.doa
    }.otherwise{
      dout := ram.io.dob
    }
  }

  val first = RegInit(true.B)
  when(io.out(0).fire) {
    when(io.out(0).bits.last) {
      first := true.B
    }.otherwise {
      first := false.B
    }
  }

  val probs = Seq(state0, state1, state2, state3).map(StaticStateMap(_))

  for(i <- 0 until 4) {
    if(i == 0)
      io.out(i).bits.prob := Mux(first, 2048.U, probs(i))
    else 
      io.out(i).bits.prob := probs(i)

    io.out(i).bits.bit := nibble_dd(3 - i)
    io.out(i).bits.last := last_dd
    io.out(i).valid := valid_dd && ~outSel && pipelineReady
    
    io.out(i + 4).bits.prob := probs(i)
    io.out(i + 4).bits.bit := nibble_dd(3 - i)
    io.out(i + 4).bits.last := last_dd
    io.out(i + 4).valid := valid_dd && outSel && pipelineReady
  }

  io.in.ready := pipelineReady
  io.status := ramInit.io.status
}

class Order1Context extends Module {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleBundle()))
    val out = DecoupledIO(new NibbleCtxBundle(12))
  })

  val outValid = RegEnable(io.in.valid, false.B, io.in.ready)
  val inReady = io.out.ready || ~outValid

  // datapath
  val ctx = RegInit(0.U(12.W))
  val ctxNxt = RegInit(0.U(12.W))
  val nibble = Reg(UInt(4.W))
  val last = RegInit(false.B)
  val last_d = RegInit(false.B)

  when(io.in.fire) {
    ctx := ctxNxt
    ctxNxt := Cat(ctxNxt(7, 0), io.in.bits.nibble)
    nibble := io.in.bits.nibble
    last := io.in.bits.last
    last_d := last
  }

  when(io.out.fire && io.out.bits.last && last_d) {
    ctx := 0.U
    ctxNxt := 0.U
  }

  io.in.ready := inReady

  io.out.bits.context := ctx
  io.out.bits.nibble := nibble
  io.out.bits.last := last
  io.out.valid := outValid
}


class Order1 extends Module {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleBundle()))
    val out = Vec(8, DecoupledIO(new BitProbBundle()))

    val status = new StatusBundle
  })

  val contextGen = Module(new Order1Context()).io
  val contextMap = Module(new ContextMap(12)).io

  contextGen.in <> io.in
  contextMap.in <> contextGen.out
  (0 until 8).map(i => io.out(i) <> contextMap.out(i))

  io.status := contextMap.status
}