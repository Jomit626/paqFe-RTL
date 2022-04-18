package paqFe.models

import chisel3._
import chisel3.util._

import paqFe.util._
import paqFe.state.StateShiftLut
import paqFe.state.StaticStateMap

import paqFe.types._

class ContextMapOutputBundle extends Bundle {
  val bits = Vec(4, UInt(1.W))
  val probs = Vec(4, UInt(12.W))

  val states = Vec(4, UInt(8.W)) // for CAS

  val hit = Bool()
  val last = Bool()
}

class ContextMap(CtxWidth : Int) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleCtxBundle(CtxWidth)))
    val out = DecoupledIO(new ContextMapOutputBundle())

    val status = new StatusBundle
  })

  val pipelineReady = Wire(Bool())

  val nibble = io.in.bits.nibble
  val context = io.in.bits.context
  val chk = io.in.bits.chk
  val last = io.in.bits.last
  val valid = io.in.fire

  val nibble_d = RegEnable(nibble, pipelineReady)
  val context_d = RegEnable(context, pipelineReady)
  val chk_d = RegEnable(chk, pipelineReady)
  val last_d = RegEnable(last, false.B, pipelineReady)
  val valid_d = RegEnable(valid, false.B, pipelineReady)

  val nibble_dd = RegEnable(nibble_d, pipelineReady)
  val context_dd = RegEnable(context_d, pipelineReady)
  val chk_dd = RegEnable(chk_d, pipelineReady)
  val last_dd = RegEnable(last_d, false.B, pipelineReady)
  val valid_dd = RegEnable(valid_d, false.B, pipelineReady)

  val harzared1 = context_d === context_dd && valid_d && valid_dd
  val harzared2 = context === context_dd && valid && valid_dd
  val harzared2_d = RegEnable(harzared2, false.B, pipelineReady)

  // duel port, A write first, B read only
  val ram = Module(new TDPRamWriteFirst(Vec(16, UInt(8.W)), CtxWidth))
  val ramInit = Module(new RamInitUnit(CtxWidth))

  ram.io.enb := io.in.valid && pipelineReady
  ram.io.web := false.B
  ram.io.dinb := DontCare
  ram.io.addrb := context

  ramInit.io.in.bits := last_dd & last_d
  ramInit.io.in.valid := valid_dd && pipelineReady

  val dout = Reg(Vec(16, UInt(8.W)))
  
  val checksum = dout(0)
  val checksumNxt = chk_dd
  val hit = checksum === checksumNxt

  val lineBytes = VecInit.tabulate(16) {i => Mux(hit, dout(i), 0.U)}

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
  val update = Cat(Seq(state3OH,state2OH,state1OH,1.U(1.W),1.U(1.W)))

  val lineNxt =  VecInit.tabulate(16) {i => Mux(update(i), dinBytes(i), lineBytes(i))}

  ram.io.ena := (valid_dd & pipelineReady) || ramInit.io.wen
  ram.io.wea := valid_dd || ramInit.io.wen
  ram.io.addra := Mux(ramInit.io.status.initDone, context_dd, ramInit.io.waddr)
  ram.io.dina := Mux(ramInit.io.status.initDone,lineNxt, 0.U.asTypeOf(ram.io.dina))
  
  pipelineReady := io.out.ready

  when(pipelineReady) {
    when(harzared1) {
      dout := lineNxt
    }.elsewhen(harzared2_d) {
      dout := ram.io.doa
    }.otherwise{
      dout := ram.io.dob
    }
  }
  val states = VecInit(Seq(state0, state1, state2, state3))
  val probs = VecInit(states.map(StaticStateMap(_)))

  io.out.bits.bits := VecInit.tabulate(4){i => nibble_dd(3 - i)}
  io.out.bits.probs := probs
  io.out.bits.states := states
  io.out.bits.hit := hit
  io.out.bits.last := last_dd
  io.out.valid := valid_dd && pipelineReady


  io.in.ready := pipelineReady
  io.status := ramInit.io.status
}
/*
class ContextMapCascadeBundel extends Bundle {
  val states = Vec(4, UInt(8.W))
  val bits = Vec(4, UInt(1.W))
  val last = Bool()
}

private class ContextMapCascade(CtxWidth : Int, CascadeNumber : Int, ID : Int) extends Module {
  val LocalBits = log2Up(CascadeNumber)
  val LocalCtxWidth = CtxWidth - LocalBits;

  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleCtxBundle(CtxWidth)))
    val inCascade = Flipped(DecoupledIO(new ContextMapCascadeBundel()))

    val out = DecoupledIO(new ContextMapCascadeBundel())
    val outCascade = DecoupledIO(new NibbleCtxBundle(CtxWidth))

    val status = new StatusBundle
  })
  
  val idBits = io.in.bits.context(CtxWidth - 1, CtxWidth - LocalBits)
  val localMask = idBits === ID.U
  val inValid = io.in.valid && localMask
  val last = io.in.valid && io.in.bits.last
  // datapath
  val cm = Module(new ContextMap(LocalCtxWidth)).io
  cm.in <> io.in
  io.in.ready 
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
    val in = DecoupledIO(ValidIO(new NibbleCtxBundle(CtxWidth)))
    val out = DecoupledIO(Vec(4, new BitProbBundle()))

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
*/

class ContextMapsToModel(n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(new ContextMapOutputBundle)))

    val out = Vec(n, Vec(8, DecoupledIO(new BitProbBundle())))
    val outCtx = Vec(8, DecoupledIO(UInt(log2Ceil(n + 1).W)))
  })

  // ctrl singal
  val inReady = Wire(Bool())
  val outValid0 = Wire(Bool())
  val outValid1 = Wire(Bool())
  
  val first = RegInit(true.B)

  // data path
  val ctx = PopCount(io.in.map(_.bits.hit))

  for(j <- 0 until 4) {
    for(i <- 0 until n) {
      io.out(i)(j).bits.bit := io.in(i).bits.bits(j)
      io.out(i)(j).bits.prob := io.in(i).bits.probs(j)
      io.out(i)(j).bits.last := io.in(i).bits.last
      io.out(i)(j).valid := outValid0

      io.out(i)(j + 4).bits.bit := io.in(i).bits.bits(j)
      io.out(i)(j + 4).bits.prob := io.in(i).bits.probs(j)
      io.out(i)(j + 4).bits.last := io.in(i).bits.last
      io.out(i)(j + 4).valid := outValid1

      if(j == 0)
        io.out(i)(j).bits.prob := Mux(first, 2048.U, io.in(i).bits.probs(j))
    }
    if(j == 0) {
      io.outCtx(j).bits :=  Mux(first, 0.U, ctx)
    } else {
      io.outCtx(j).bits :=  ctx
    }
    io.outCtx(j).valid := outValid0

    io.outCtx(j + 4).bits := ctx
    io.outCtx(j + 4).valid := outValid1
  }

  for(i <- 0 until n) {
    io.in(i).ready := inReady
  }

  // ctral singal gen
  when(first && io.out(0)(0).fire) {
    first := false.B
  }
  when(~first && io.out(0)(4).fire && io.out(0)(4).bits.last) {
    first := true.B
  }

  val outSel = RegInit(false.B)
  when(io.in.last.fire) {
    outSel := ~outSel
  }

  val outHalf0 = io.out.map(_.slice(0, 4)).reduce(_ ++ _)
  val outHalf1 = io.out.map(_.slice(4, 8)).reduce(_ ++ _)
  val outCtxhalf0 = io.outCtx.slice(0, 4)
  val outCtxhalf1 = io.outCtx.slice(4, 8)

  val outHalf0Ready = outHalf0.map(_.ready).reduce(_ && _)
  val outHalf1Ready = outHalf1.map(_.ready).reduce(_ && _)
  val outCtxHalf0Ready = outCtxhalf0.map(_.ready).reduce(_ && _)
  val outCtxHalf1Ready = outCtxhalf1.map(_.ready).reduce(_ && _)

  val inValid = io.in.map(_.valid).reduce(_ && _)
  inReady := Mux(outSel, outHalf1Ready && outCtxHalf1Ready, outHalf0Ready && outCtxHalf0Ready)

  outValid0 := ~outSel && inReady && inValid
  outValid1 := outSel && inReady && inValid
}
