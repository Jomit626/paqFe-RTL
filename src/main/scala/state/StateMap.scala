package paqFe.state

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.RegEnable
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.conversions._

import paqFe.types._
import paqFe.util.RamInitUnit

class StateInfo extends Bundle {
  val cnt = UInt(10.W)
  val prob = UInt(22.W)
}

class StateMapInputBundle(bits: Int) extends Bundle {
  val state = UInt(bits.W)
  val bit = UInt(1.W)
  val last = Bool()
}

class StateMapProbUpdate extends Module {
  val io = IO(new Bundle {
    val info = Input(new StateInfo())
    val bit = Input(UInt(1.W))

    val info_next = Output(new StateInfo())
  })
  val cnt = io.info.cnt
  val prob = io.info.prob
  val bit = io.bit

  val scale = VecInit((0 until 1024) map (i => (16384 / (2 * i + 3)).U(13.W)))
  val delta = (bit << 22.U).asSInt - prob.asSInt
  val dp = ((delta >> 3) * scale(cnt)) >> 10
  val probNext = prob.asSInt + dp
  
  io.info_next.cnt := Mux(cnt === 1023.U, cnt, cnt +& 1.U)
  io.info_next.prob := probNext.asUInt
}

object StateMapProbUpdate {
  def apply(info: StateInfo, bit: UInt) = {
    val m = Module(new StateMapProbUpdate)
    m.io.bit := bit
    m.io.info := info

    m.io.info_next
  }
}

class StateMap(bits: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new StateMapInputBundle(bits)))
    val out = DecoupledIO(new BitProbBundle())

    val status = Output(new StatusBundle)
  })
  // ctrl singal
  val inReady = WireInit(false.B)
  val outValid = WireInit(false.B)

  val pipeReady = io.out.ready

  // data path
  val state = RegEnable(io.in.bits.state, pipeReady)
  val bit = RegEnable(io.in.bits.bit, pipeReady)
  val last = RegEnable(io.in.bits.last, pipeReady)
  val valid = RegEnable(io.in.valid, pipeReady)
  
  val forwared = RegNext(io.in.bits.state === state && valid, pipeReady)

  val ram = SyncReadMem(1 << bits, new StateInfo)
  val ramInit = Module(new RamInitUnit(bits))
  ramInit.io.in.bits := last
  ramInit.io.in.valid := io.out.fire

  val InitInfo = (new StateInfo).Lit(
    _.cnt -> 0.U,
    _.prob -> (1 << 21).U
  )
  val infoUpdate = Wire(new StateInfo)
  val wdata = Mux(ramInit.io.status.initDone, infoUpdate, InitInfo)
  val waddr = Mux(ramInit.io.status.initDone, state, ramInit.io.waddr)
  val wen = ramInit.io.wen | valid
  when(wen) { ram.write(waddr, wdata) }

  val dout = ram.read(io.in.bits.state, io.in.fire)
  val infoForwarded = RegEnable(wdata, wen)
  val info = Mux(forwared, infoForwarded, dout)

  infoUpdate := StateMapProbUpdate(info, bit)

  // ctrl singal gen
  inReady := io.out.ready || ~valid
  outValid := valid

  // IO
  io.in.ready := inReady

  io.out.valid := outValid
  io.out.bits.bit := bit
  io.out.bits.last := last
  io.out.bits.prob := info.prob >> 10.U

  io.status := ramInit.io.status
}

