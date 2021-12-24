package ram

import chisel3._
import chisel3.util._

object GrayCounter {
  def apply(width : Int, inc : Bool) : (UInt, UInt) = {
    val binNxt = Wire(UInt(width.W))
    val bin = RegNext(binNxt, 0.U(width.W))
    binNxt := bin + inc.asUInt

    (bin(width-1, 0), binNxt ^ (binNxt >> 1.U))
  }
}

object Synchronizer {
  def apply[T <: Data](t : T) : T = {
    val tD = RegNext(t, 0.U)
    val tDD = RegNext(tD, 0.U)

    tDD.asTypeOf(t)
  }
}

class AsyncQueueSource[T <: Data](gen : T, AddrWidth : Int) extends Module {
  require(AddrWidth > 0)

  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(gen))

    // to mem
    val wen = Output(Bool())
    val waddr = Output(UInt(AddrWidth.W))
    val wdata = Output(gen)

    // to/from sink
    val wgray = Output(UInt((AddrWidth + 1).W))
    val rgray = Input(UInt((AddrWidth + 1).W))
  })

  val wen = io.enq.fire
  val (waddr, wgray) = GrayCounter(AddrWidth + 1, wen)
  val rgray = Synchronizer(io.rgray)

  val wfull = wgray =/= Cat(~rgray(AddrWidth, AddrWidth-1), rgray(AddrWidth-2,0))

  io.enq.ready := RegNext(wfull, false.B)

  io.wen := wen
  io.waddr := waddr
  io.wdata := io.enq.bits

  io.wgray := wgray
}

class AsyncQueueSink[T <: Data](gen : T, AddrWidth : Int) extends Module {
  require(AddrWidth > 0)

  val io = IO(new Bundle {
    val deq = DecoupledIO(gen)

    // to/from mem
    val raddr = Output(UInt(AddrWidth.W))
    val rdata = Input(gen)

    // to/from sink
    val wgray = Input(UInt((AddrWidth + 1).W))
    val rgray = Output(UInt((AddrWidth + 1).W))
  })

  val ren = io.deq.fire
  val (raddr, rgray) = GrayCounter(AddrWidth, ren)
  val wgray = Synchronizer(io.wgray)

  val rempty = rgray === wgray

  io.deq.valid := RegNext(rempty, false.B)
  io.deq.bits := io.rdata

  io.raddr := raddr
  
  io.rgray := rgray
}

class DPRam[T <: Data](gen : T, Depth : Int) extends RawModule {
  require(Depth > 0 && isPow2(Depth))
  val AddrWidth = log2Ceil(Depth)

  val io = IO(new Bundle {
    val wclk = Input(Clock())
    val wen = Input(Bool())
    val waddr = Input(UInt(AddrWidth.W))
    val wdata = Input(gen)

    val raddr = Input(UInt(AddrWidth.W))
    val rdata = Output(gen)
  })

  withClock(io.wclk) {
    val mem = Reg(Vec(Depth, gen))
    when(io.wen) {
      mem(io.waddr) := io.wdata
    }

    io.rdata := mem(io.raddr)
  }
}

class AsyncQueue[T <: Data](gen : T, Depth : Int) extends Module {
  require(Depth > 0 && isPow2(Depth))
  val AddrWidth = log2Ceil(Depth)
  val io = IO(new Bundle {
    val enq_clock = Input(Clock())
    val enq_reset = Input(Bool())

    val enq = Flipped(DecoupledIO(gen))

    val deq_clock = Input(Clock())
    val deq_reset = Input(Bool())

    val deq = DecoupledIO(gen)
  })

  val sink = Module(new AsyncQueueSink(gen, AddrWidth))
  val source = Module(new AsyncQueueSource(gen, AddrWidth))
  val mem = Module(new DPRam(gen, Depth))

  mem.io.wclk := io.enq_clock
  mem.io.wen := source.io.wen
  mem.io.waddr := source.io.waddr
  mem.io.wdata := source.io.wdata

  mem.io.raddr := sink.io.raddr
  sink.io.rdata := mem.io.rdata

  sink.clock := io.enq_clock
  sink.reset := io.enq_reset

  source.clock := io.deq_clock
  source.reset := io.deq_reset

  source.io.rgray := sink.io.rgray
  sink.io.wgray := source.io.waddr

  io.enq <> source.io.enq
  sink.io.deq <> io.deq
}

import chisel3.stage.ChiselStage
object VerilogMain extends App {
  (new ChiselStage).emitVerilog(new AsyncQueue(UInt(8.W), 16))
}