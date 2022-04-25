package paqFe.util

import chisel3._
import chisel3.util._

import paqFe.types.StatusBundle

class TDPRamWFRO[T <: Data](gen: T, width: Int) extends Module {
  val io = IO(new Bundle {
    val ena = Input(Bool())
    val wea = Input(Bool())
    val addra = Input(UInt(width.W))
    val dina = Input(gen)
    val doa = Output(gen)

    val enb = Input(Bool())
    val addrb = Input(UInt(width.W))
    val dob = Output(gen)
  })

  val mem = Mem(1 << width, gen)
  val doa = Reg(gen)
  when(io.ena) {
    when(io.wea) {
      mem(io.addra) := io.dina
      doa := io.dina
    }otherwise {
      doa := mem(io.addra)
    }
  }

  val dob = Reg(gen)
  when(io.enb) {
    dob := mem(io.addrb)
  }

  io.doa := doa
  io.dob := dob
}

class RamInitUnit(AddrWidth : Int, delay: Int = 2) extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(Bool()))

    val wen = Output(Bool())
    val waddr = Output(UInt(AddrWidth.W))

    val status = Output(new StatusBundle)
  })

  val initDone = RegInit(false.B)
  val (cnt, cntDone) = Counter(0 until (1 << AddrWidth), ~initDone)

  initDone := (initDone || cntDone) && ~(io.in.valid && io.in.bits)

  io.wen := ShiftRegister(~initDone, delay)
  io.waddr := ShiftRegister(cnt, delay)

  io.status.initDone := ShiftRegister(initDone, delay)
}
