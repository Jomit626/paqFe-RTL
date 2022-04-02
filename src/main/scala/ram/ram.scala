package paqFe.ram

import chisel3._
import chisel3.util._

import paqFe.types.StatusBundle

private class ByteWriteDPRamBL(col : Int, addr_width: Int) extends BlackBox(
  Map("NUM_COL" -> col,
      "ADDR_WIDTH" -> addr_width)) with HasBlackBoxResource {
  
  val data_width = 8 * col
  val io = IO(new Bundle {
    val clk = Input(Clock())

    val ena = Input(Bool())
    val wea = Input(UInt(col.W))
    val addra = Input(UInt(addr_width.W))
    val dina = Input(UInt(data_width.W))

    val enb = Input(Bool())
    val addrb = Input(UInt(addr_width.W))
    val dob = Output(UInt(data_width.W))
  })

  addResource("/bytewrite_dp_ram.v")
}

class ByteWriteDPRam(col : Int, addr_width: Int) extends Module {
  
  val data_width = 8 * col
  val io = IO(new Bundle {
    val ena = Input(Bool())
    val wea = Input(UInt(col.W))
    val addra = Input(UInt(addr_width.W))
    val dina = Input(UInt(data_width.W))

    val enb = Input(Bool())
    val addrb = Input(UInt(addr_width.W))
    val dob = Output(UInt(data_width.W))
  })

  private val inst = Module(new ByteWriteDPRamBL(col, addr_width))

  inst.io.ena := io.ena
  inst.io.wea := io.wea
  inst.io.addra := io.addra
  inst.io.dina := io.dina

  inst.io.enb := io.enb
  inst.io.addrb := io.addrb
  io.dob := inst.io.dob

  inst.io.clk := clock
}

private class ByteWriteTDPRamBL(col : Int, addr_width: Int) extends BlackBox(
  Map("NUM_COL" -> col,
      "ADDR_WIDTH" -> addr_width)) with HasBlackBoxResource {
  
  val data_width = 8 * col
  val io = IO(new Bundle {
    val clk = Input(Clock())

    val ena = Input(Bool())
    val wea = Input(UInt(col.W))
    val addra = Input(UInt(addr_width.W))
    val dina = Input(UInt(data_width.W))
    val doa = Output(UInt(data_width.W))

    val enb = Input(Bool())
    val addrb = Input(UInt(addr_width.W))
    val dob = Output(UInt(data_width.W))
  })

  addResource("/bytewrite_tdp_ram.v")
}

class ByteWriteTDPRam(col : Int, addr_width: Int) extends Module {
  
  val data_width = 8 * col
  val io = IO(new Bundle {
    val ena = Input(Bool())
    val wea = Input(UInt(col.W))
    val addra = Input(UInt(addr_width.W))
    val dina = Input(UInt(data_width.W))
    val doa = Output(UInt(data_width.W))

    val enb = Input(Bool())
    val addrb = Input(UInt(addr_width.W))
    val dob = Output(UInt(data_width.W))
  })

  private val inst = Module(new ByteWriteTDPRamBL(col, addr_width))

  inst.io.ena := io.ena
  inst.io.wea := io.wea
  inst.io.addra := io.addra
  inst.io.dina := io.dina
  io.doa := inst.io.doa

  inst.io.enb := io.enb
  inst.io.addrb := io.addrb
  io.dob := inst.io.dob

  inst.io.clk := clock
}

class ByteWriteTDPRamFWB(col : Int, addr_width: Int) extends Module {
  
  val data_width = 8 * col
  val io = IO(new Bundle {
    val ena = Input(Bool())
    val wea = Input(UInt(col.W))
    val addra = Input(UInt(addr_width.W))
    val dina = Input(UInt(data_width.W))

    val enb = Input(Bool())
    val addrb = Input(UInt(addr_width.W))
    val dob = Output(UInt(data_width.W))
  })

  private val inst = Module(new ByteWriteTDPRam(col, addr_width))

  val fw = RegNext(io.addra === io.addrb && io.ena, false.B)

  inst.io.ena := io.ena
  inst.io.wea := io.wea
  inst.io.addra := io.addra
  inst.io.dina := io.dina

  inst.io.enb := io.enb
  inst.io.addrb := io.addrb
  io.dob := Mux(fw, inst.io.doa, inst.io.dob) 

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
