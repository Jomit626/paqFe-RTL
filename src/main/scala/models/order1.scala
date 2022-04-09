package paqFe.models

import chisel3._
import chisel3.util._

import paqFe.ram._
import paqFe.state.StateShiftLut
import paqFe.state.StaticStateMap

import paqFe.types._

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

  val lineBytes = (0 until 16).map(i => Mux(hit, dout(8*i+7,8*i), 0.U))

  val state0 = lineBytes(1)
  val state0Nxt = StateShiftLut(state0, nibble_dd(3))

  val state1group = (2 until 4).map(lineBytes(_))
  val state1OH = UIntToOH(nibble_dd(3))
  val state1 = Mux1H(state1OH, state1group)
  val state1Nxt = StateShiftLut(state1, nibble_dd(2))

  val state2group = (4 until 8).map(lineBytes(_))
  val state2OH = UIntToOH(nibble_dd(3,2))
  val state2 = Mux1H(state2OH, state2group)
  val state2Nxt = StateShiftLut(state2, nibble_dd(1))

  val state3group = (8 until 16).map(lineBytes(_))
  val state3OH = UIntToOH(nibble_dd(3,1))
  val state3 = Mux1H(state3OH, state3group)
  val state3Nxt = StateShiftLut(state3, nibble_dd(0))

  val dinBytes = Seq(checksumNxt) ++ Seq(state0Nxt) ++ Seq.fill(2){state1Nxt} ++ Seq.fill(4){state2Nxt} ++ Seq.fill(8){state3Nxt}
  val wen = Cat(Seq(state3OH,state2OH,state1OH,1.U(1.W),1.U(1.W)))

  val lineNxt = (0 until 16).map(i => Mux(wen(i), dinBytes(i), lineBytes(i)))

  ram.io.ena := (valid_dd & pipelineReady) | ramInit.io.wen
  ram.io.wea := wen | Cat(Seq.fill(wen.getWidth){ramInit.io.wen})
  ram.io.addra := Mux(ramInit.io.status.initDone, context_dd, ramInit.io.waddr)
  ram.io.dina := Mux(ramInit.io.status.initDone, Cat(dinBytes.reverse), 0.U)
  
  val outSel = RegInit(false.B)
  when(valid_dd && pipelineReady) {
    outSel := ~outSel
  }
  pipelineReady := Mux(outSel, io.out.slice(4,8).map(_.ready).reduce(_ && _), io.out.slice(0,4).map(_.ready).reduce(_ && _))

  when(pipelineReady) {
    when(harzared1) {
      dout := Cat(lineNxt.reverse)
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

private class ContextMapCascade(CtxWidth : Int, CascadeNumber : Int, ID : Int) extends Module {
  val LocalBits = log2Up(CascadeNumber)
  val LocalCtxWidth = CtxWidth - LocalBits;

  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleCtxBundle(CtxWidth)))
    val inCascade = Flipped(ValidIO(Vec(4, new BitProbBundle())))

    val out = ValidIO(Vec(4, new BitProbBundle()))
    val outCascade = ValidIO(new NibbleCtxBundle(CtxWidth))

    val status = new StatusBundle
  })
  
  val idBits = io.in.bits.context(CtxWidth - 1, CtxWidth - LocalBits)
  val inValid = io.in.valid && idBits === ID.U
  val last = io.in.valid && io.in.bits.last
  
  val cm = Module(new ContextMap(LocalCtxWidth)).io
  cm.in := io.in
  cm.in.valid := inValid || last

  if(ID == 0) {
    io.out := cm.out
  } else {
    val inCascade = Pipe(io.inCascade, 3)

    val output = inValid
    val output_d = RegNext(output, false.B)
    val output_dd = RegNext(output_d, false.B)

    io.out := Mux(output_dd, cm.out, inCascade)
  }
  
  io.outCascade := Pipe(io.in, 3)
  io.status := cm.status
}

class ContextMapLarge(CtxWidth : Int, CascadeNumber : Int) extends Module {

  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleCtxBundle(CtxWidth)))
    val out = ValidIO(Vec(4, new BitProbBundle()))

    val status = new StatusBundle
  })

  val cms = (0 until CascadeNumber) map {i => Module(new ContextMapCascade(CtxWidth, CascadeNumber, i)).io}

  cms.head.in := io.in
  cms.head.inCascade := DontCare

  cms.dropRight(1) zip cms.drop(1) map {case (cm_out, cm_in) =>
    cm_in.in := cm_out.outCascade
    cm_in.inCascade := cm_out.out
  }

  io.out := cms.last.out
  io.status := StatusMerge(cms map (_.status))
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

/*
class OrdersContext extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleBundle()))
    val out = ValidIO(Vec(4,new NibbleCtxBundle(12)))
  })

  val C = RegInit()

  val ctxNxt = Wire(UInt())
  val ctx = RegEnable(ctxNxt, 0.U, io.in.valid)

  val last = io.in.bits.last
  val last_d = RegNext(last, false.B)

  ctxNxt := Mux(last && last_d, 0.U, Cat(ctx(7, 0), io.in.bits.nibble))

  io.out.bits.context := ctx
  io.out.bits.nibble := io.in.bits.nibble
  io.out.bits.last := io.in.bits.last
  io.out.valid := io.in.valid
}
*/

object ContextMap2Model {
  def apply(in : Valid[Vec[BitProbBundle]] ) = {
    require(in.bits.length == 4)
    val out = Wire(Vec(8, Valid(new BitProbBundle())))

    // used to force set first prob to 2048
    val first = RegInit(true.B)
    when(first && out(0).valid) {
      first := false.B
    }
    when(~first && out(4).valid && out(4).bits.last) {
      first := true.B
    }

    val outSel = RegInit(false.B)
    when(in.valid) {
      outSel := ~outSel
    }
    
    (0 until 4).map(i =>  {
      out(i).bits := in.bits(i)
      out(i).valid := in.valid && ~outSel
      out(i + 4).bits := in.bits(i)
      out(i + 4).valid := in.valid && outSel

      if(i == 0)
        out(i).bits.prob := Mux(first, 2048.U, in.bits(i).prob)
    })

    out
  }
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