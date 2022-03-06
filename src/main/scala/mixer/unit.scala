package paqFe.mixer

import chisel3._
import chisel3.util._
import internal.firrtl.Width

class DPRam[T <: Data](t : T, addr : Width) extends Module {
  val io = IO(new Bundle {
    val wea = Input(Bool())
    val addra = Input(UInt(addr))
    val dina = Input(t)

    val enb = Input(Bool())
    val addrb = Input(UInt(addr))
    val dob = Output(t)
  })

  val mem = SyncReadMem(1 << addr.get, t)

  io.dob := mem.read(io.addrb, io.enb)

  when(io.wea) {
    mem.write(io.addra, io.dina)
  }
}

class MACC(AWidth : Width, BWidth : Width, CWidth : Width) extends Module {
  val latency = 4

  val io = IO(new Bundle {
    val ce = Input(Bool())

    val a = Input(SInt(AWidth))
    val b = Input(SInt(BWidth))
    val reload = Input(Bool())

    val acc = Output(SInt(CWidth))
  })

  val a = RegEnable(io.a, io.ce)
  val b = RegEnable(io.b, io.ce)
  val reload = ShiftRegister(io.reload, 2 , true.B, io.ce)

  val mul = RegEnable(a * b, io.ce)
  val lastResult = Wire(SInt(CWidth))
  val acc = RegEnable(mul + lastResult, io.ce)

  lastResult := Mux(reload, 0.S, acc)

  io.acc := RegEnable(acc, io.ce)
}

// c - a * b
class MS(AWidth : Width, BWidth : Width, CWidth : Width) extends Module {
  val latency = 3

  val io = IO(new Bundle {
    val ce = Input(Bool())

    val a = Input(SInt(AWidth))
    val b = Input(SInt(BWidth))
    val c = Input(SInt(CWidth))

    val out = Output(SInt(CWidth))
  })

  val a = RegEnable(io.a, io.ce)
  val b = RegEnable(io.b, io.ce)
  val c = ShiftRegister(io.c, 2, io.ce)

  val mul = RegEnable(a * b, io.ce)
  val out = RegEnable((c << 16) + mul, io.ce) >> 16

  io.out := out
}