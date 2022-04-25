package paqFe.models

import chisel3._
import chisel3.util._

import paqFe.util._
import paqFe.state.StateShiftLut
import paqFe.state.StaticStateMap

import paqFe.types._

class ContextMapInputBundle(CtxWidth : Int) extends Bundle {
  val nibble = UInt(4.W)
  val context = UInt(CtxWidth.W)
  val chk = UInt(8.W)
  val last = Bool()
}

class ContextMapOutputBundle() extends Bundle {
  val nibble = UInt(4.W)
  val probs = Vec(4, UInt(12.W))
  val hit = Bool()
  val context = UInt()
  val last = Bool()
}

class ContextMapCASBundle(CtxWidth : Int) extends Bundle {
  val nibble = UInt(4.W)
  val context = UInt(CtxWidth.W)
  val chk = UInt(8.W)

  val states = Vec(4, UInt(8.W))
  val hit = Bool()
  
  val last = Bool()
}

class ContextMapPE(CtxWidth: Int, CascadeNumber: Int, ID: Int) extends Module {
  require(isPow2(CascadeNumber) && ID >= 0 && ID < CascadeNumber)
  val LocalBits = log2Ceil(CascadeNumber)
  val LocalCtxWidth = CtxWidth - LocalBits;

  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new ContextMapCASBundle(CtxWidth)))
    val out = DecoupledIO(new ContextMapCASBundle(CtxWidth))

    val status = Output(new StatusBundle)
  })
  val latency = 2
  
  val pipelineReady = Wire(Bool())
  val localMask = if(LocalBits != 0) {
    val idBits = io.in.bits.context(CtxWidth - 1, CtxWidth - LocalBits)
    idBits === ID.U
  } else {
    true.B
  }

  val nibble = io.in.bits.nibble
  val context = io.in.bits.context
  val chk = io.in.bits.chk
  val last = io.in.bits.last
  val valid = io.in.fire
  val localValid = io.in.fire && localMask

  val nibble_d = RegEnable(nibble, pipelineReady)
  val context_d = RegEnable(context, pipelineReady)
  val chk_d = RegEnable(chk, pipelineReady)
  val last_d = RegEnable(last, false.B, pipelineReady)
  val valid_d = RegEnable(valid, false.B, pipelineReady)
  val localValid_d = RegEnable(localValid, false.B, pipelineReady)

  val nibble_dd = RegEnable(nibble_d, pipelineReady)
  val context_dd = RegEnable(context_d, pipelineReady)
  val chk_dd = RegEnable(chk_d, pipelineReady)
  val last_dd = RegEnable(last_d, false.B, pipelineReady)
  val valid_dd = RegEnable(valid_d, false.B, pipelineReady)
  val localValid_dd = RegEnable(localValid_d, false.B, pipelineReady)

  val harzared1 = context_d === context_dd && localValid_d && localValid_dd
  val harzared2 = context === context_dd && localValid && localValid_dd
  val harzared2_d = RegEnable(harzared2, false.B, pipelineReady)

  // duel port, A write first, B read only
  val ram = Module(new TDPRamWFRO(UInt((16 * 8).W), LocalCtxWidth))
  val ramInit = Module(new RamInitUnit(LocalCtxWidth))

  ram.io.enb := localValid && pipelineReady
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

  ram.io.ena := (localValid_dd & pipelineReady) || ramInit.io.wen
  ram.io.wea := localValid_dd || ramInit.io.wen
  ram.io.addra := Mux(ramInit.io.status.initDone, context_dd, ramInit.io.waddr)
  ram.io.dina := Mux(ramInit.io.status.initDone, lineNxt.asUInt, 0.U.asTypeOf(ram.io.dina))
  
  pipelineReady := io.out.ready

  when(pipelineReady) {
    when(harzared1) {
      dout := lineNxt
    }.elsewhen(harzared2_d) {
      dout := ram.io.doa.asTypeOf(dout)
    }.otherwise{
      dout := ram.io.dob.asTypeOf(dout)
    }
  }
  val states = VecInit(Seq(state0, state1, state2, state3))
  val statesIn = ShiftRegister(io.in.bits.states, latency, pipelineReady)
  val hitIn = ShiftRegister(io.in.bits.hit, latency, pipelineReady)

  io.out.bits.nibble := nibble_dd
  io.out.bits.context := context_dd
  io.out.bits.chk := chk_dd
  io.out.bits.states := Mux(localValid_dd, states, statesIn)
  io.out.bits.hit := Mux(localValid_dd, hit, hitIn)
  io.out.bits.last := last_dd
  io.out.valid := valid_dd

  io.in.ready := pipelineReady
  io.status := ramInit.io.status
}

class ContextMap(CtxWidth: Int, PECtxWidth: Int = 14) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new NibbleCtxBundle(CtxWidth)))

    val out = DecoupledIO(new ContextMapOutputBundle())

    val status = Output(new StatusBundle)
  })

  val CascadeNumber = 1 << (CtxWidth - PECtxWidth)
  val PEs = Seq.tabulate(CascadeNumber){i =>
    Module(new ContextMapPE(CtxWidth, CascadeNumber, i))
  }
  
  PEs.head.io.in <> DecoupledMap(io.in, {i: NibbleCtxBundle =>
    val o = Wire(new ContextMapCASBundle(CtxWidth))
    o.context := i.context
    o.chk := i.chk
    o.nibble := i.nibble
    o.last := i.last
    
    o.hit := DontCare
    o.states := DontCare
    o
  })
  
  PEs.dropRight(1) zip PEs.drop(1) map {case (in, out) =>
    in.io.out <> out.io.in
  }
  
  io.out <> DecoupledMap(DecoupledSkidBuf(PEs.last.io.out), {i: ContextMapCASBundle =>
    val o = Wire(new ContextMapOutputBundle())
    o.nibble := i.nibble
    o.last := i.last
    o.context := i.context
    o.probs := i.states.map(StaticStateMap(_))
    o.hit := i.hit
    o
  })
  val latency = PEs.map(_.latency).reduce(_ + _)

  io.status := StatusMerge(PEs map (_.io.status))
}

class ContextMapsToModel(n: Int) extends Module {
  val nCtx = 6
  val io = IO(new Bundle {
    val in = Vec(n, Flipped(DecoupledIO(new ContextMapOutputBundle)))

    val out = Vec(n, Vec(8, DecoupledIO(new BitProbBundle())))
    val outCtx = Vec(nCtx, Vec(8, DecoupledIO(UInt(8.W))))
  })

  // ctrl singal
  val inReady = Wire(Bool())
  val outValid0 = Wire(Bool())
  val outValid1 = Wire(Bool())
  
  val first = RegInit(true.B)
  val reload = WireInit(false.B)

  // data path
  val salt = RegInit(0.U(2.W))
  val hitSeq = io.in.map(_.bits.hit & ~first)
  val hitVec = Cat(hitSeq)
  val prevHitVec = RegInit(0.U(hitVec.getWidth.W))
  val prevHitVec2 = RegInit(0.U(hitVec.getWidth.W))

  val wordRunLevel = Wire(UInt(6.W))
  val prevWordRunLevel = RegInit(0.U(6.W))
  val highRunLevel = RegInit(0.U(5.W))
  val highRunLevelNxt = Wire(UInt(5.W))
  when(hitSeq(4)) {
    highRunLevelNxt := Mux(highRunLevel<31.U, highRunLevel +& 1.U, highRunLevel)
  }.otherwise{
    highRunLevelNxt := 0.U
  }

  val wordHit = hitSeq(5) && io.in(5).bits.context =/= 0.U // TODO: remove hand write 5
  wordRunLevel := Cat(prevWordRunLevel, wordHit.asUInt)

  val ctxs = VecInit(Seq(
    Cat(hitVec, salt),
    Cat(prevHitVec & hitVec, salt),
    Cat(prevHitVec2 & hitVec, salt),
    Cat(wordRunLevel, salt),
    Cat(prevWordRunLevel, salt),
    Cat(highRunLevelNxt, salt)
  ))

  when(outValid1 || outValid0) {
    when(outValid1) {salt := salt +& outValid1}
    prevHitVec := hitVec
    prevHitVec2 := prevHitVec

    prevWordRunLevel := wordRunLevel
    highRunLevel := highRunLevelNxt
  }

  when(reload) {
    salt := 0.U
    prevHitVec := 0.U
    prevHitVec2 := 0.U
    prevWordRunLevel := 0.U
    highRunLevel := 0.U
  }

  for(j <- 0 until 4) {
    for(i <- 0 until n) {
      io.out(i)(j).bits.bit := io.in(i).bits.nibble(3 - j)
      io.out(i)(j).bits.prob := io.in(i).bits.probs(j)
      io.out(i)(j).bits.last := io.in(i).bits.last
      io.out(i)(j).valid := outValid0

      io.out(i)(j + 4).bits.bit := io.in(i).bits.nibble(3 - j)
      io.out(i)(j + 4).bits.prob := io.in(i).bits.probs(j)
      io.out(i)(j + 4).bits.last := io.in(i).bits.last
      io.out(i)(j + 4).valid := outValid1

      if(j == 0)
        io.out(i)(j).bits.prob := Mux(first, 2048.U, io.in(i).bits.probs(j))
    }

    for(i <- 0 until nCtx) {
      io.outCtx(i)(j).bits := (if(j == 0) Mux(first, 0.U, ctxs(i)) else ctxs(i))
      io.outCtx(i)(j).valid := outValid0

      io.outCtx(i)(j + 4).bits := ctxs(i)
      io.outCtx(i)(j + 4).valid := outValid1
    }
  }

  for(i <- 0 until n) {
    io.in(i).ready := inReady
  }

  // ctrl singal gen
  when(first && io.out(0)(0).fire) {
    first := false.B
  }
  when(~first && io.out(0)(4).fire && io.out(0)(4).bits.last) {
    first := true.B
    reload := true.B
  }

  val outSel = RegInit(false.B)
  when(io.in.last.fire) {
    outSel := ~outSel
  }

  val outHalf0 = io.out.map(_.slice(0, 4)).reduce(_ ++ _)
  val outHalf1 = io.out.map(_.slice(4, 8)).reduce(_ ++ _)
  val outCtxhalf0 = io.outCtx.map(_.slice(0, 4)).reduce(_ ++ _)
  val outCtxhalf1 = io.outCtx.map(_.slice(4, 8)).reduce(_ ++ _)

  val outHalf0Ready = outHalf0.map(_.ready).reduce(_ && _)
  val outHalf1Ready = outHalf1.map(_.ready).reduce(_ && _)
  val outCtxHalf0Ready = outCtxhalf0.map(_.ready).reduce(_ && _)
  val outCtxHalf1Ready = outCtxhalf1.map(_.ready).reduce(_ && _)

  val inValid = io.in.map(_.valid).reduce(_ && _)
  inReady := Mux(outSel, outHalf1Ready && outCtxHalf1Ready, outHalf0Ready && outCtxHalf0Ready) && inValid

  outValid0 := ~outSel && inReady && inValid
  outValid1 := outSel && inReady && inValid
}
