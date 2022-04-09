package paqFe.models

import chisel3._
import chisel3.util._

import paqFe.ram._
import paqFe.state.StateShiftLut
import paqFe.state.StaticStateMap

import paqFe.types._


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
