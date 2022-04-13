package paqFe.util

import chisel3._
import chisel3.util._

import paqFe.types.StatusBundle

class TDPRamWriteFirst[T <: Data](gen: T, width: Int) extends Module {
  val io = IO(new Bundle {
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(width.W))
    val dina = Input(gen)
    val doa = Output(gen)

    val enb = Input(Bool())
    val web = Input(Bool())
    val addrb = Input(UInt(width.W))
    val dinb = Input(gen)
    val dob = Output(gen)
  })

  val mem = SyncReadMem(1 << width, gen, SyncReadMem.WriteFirst)
  io.doa := mem.read(io.addra, io.ena)
  when(io.ena && io.wea) {
    mem.write(io.addra, io.dina)
  }

  io.dob := mem.read(io.addrb, io.enb)
  when(io.enb && io.web) {
    mem.write(io.addrb, io.dinb)
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
