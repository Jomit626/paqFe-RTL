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
    val out = Vec(n, ValidIO(new NibbleBundle()))
  })

  val s0 = 0.U(1.W)
  val s1 = 1.U(1.W)
  val stateNxt = Wire(UInt())
  val state = RegNext(stateNxt, s0)

  stateNxt := Mux(io.in.valid, ~state, state)

  io.in.ready := state === s1

  for (i <- 0 until n) {
    io.out(i).bits.nibble := RegNext(Mux(state === s0, io.in.bits.byte(7,4), io.in.bits.byte(3,0)))
    io.out(i).bits.last := RegNext(io.in.bits.last)
    io.out(i).valid := RegNext(io.in.valid, 0.U)
  }
}

class ContextMap(CtxWidth : Int) extends Module {

  val io = IO(new Bundle{
    val in = Flipped(ValidIO(new NibbleCtxBundle(CtxWidth)))
    val out = ValidIO(Vec(4, new BitProbBundle()))

    val status = new StatusBundle
  })

  val nibble = io.in.bits.nibble
  val context = io.in.bits.context
  val last = io.in.bits.last
  val valid = io.in.valid

  val nibble_d = RegNext(nibble)
  val context_d = RegNext(context)
  val last_d = RegNext(last, false.B)
  val valid_d = RegNext(valid, false.B)

  val nibble_dd = RegNext(nibble_d)
  val context_dd = RegNext(context_d)
  val last_dd = RegNext(last_d, false.B)
  val valid_dd = RegNext(valid_d, false.B)

  val harzared1 = context_d === context_dd && valid_d && valid_dd
  val harzared2 = context === context_dd && valid && valid_dd
  val harzared2_d = RegNext(harzared2, false.B)

  // duel port, A write first, B read only
  val ram = Module(new ByteWriteTDPRam(16, CtxWidth))
  val ramInit = Module(new RamInitUnit(CtxWidth))

  ram.io.enb := io.in.valid
  ram.io.addrb := context

  ramInit.io.in.bits := last_dd
  ramInit.io.in.valid := valid_dd

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

  // val lineNxt = (din zip line).zipWithIndex.map({case ((a, b), i) => Mux(wen(i), a, b)})
  val lineNxt = (0 until 16).map(i => Mux(wen(i), dinBytes(i), lineBytes(i)))

  ram.io.ena := valid_dd | ramInit.io.wen
  ram.io.wea := wen | Cat(Seq.fill(wen.getWidth){ramInit.io.wen})
  ram.io.addra := Mux(ramInit.io.status.initDone, context_dd, ramInit.io.waddr)
  ram.io.dina := Mux(ramInit.io.status.initDone, Cat(dinBytes.reverse), 0.U)
  
  when(harzared1) {
    dout := Cat(lineNxt.reverse)
  }.elsewhen(harzared2_d) {
    dout := ram.io.doa
  }.otherwise{
    dout := ram.io.dob
  }

  val probs = Seq(state0, state1, state2, state3).map(StaticStateMap(_))

  for(i <- 0 until 4) {
    io.out.bits(i).prob := probs(i)
    io.out.bits(i).bit := nibble_dd(3 - i)
    io.out.bits(i).last := last_dd
    io.out.valid := valid_dd
  }

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
    val in = Flipped(ValidIO(new NibbleBundle()))
    val out = ValidIO(new NibbleCtxBundle(12))
  })

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
    val in = Flipped(ValidIO(new NibbleBundle()))
    val out = Vec(8, ValidIO(new BitProbBundle()))

    val status = new StatusBundle
  })

  val contextGen = Module(new Order1Context()).io
  val contextMap = Module(new ContextMap(12)).io

  contextGen.in := io.in
  contextMap.in := contextGen.out

  io.out := ContextMap2Model(contextMap.out)

  io.status := contextMap.status
}