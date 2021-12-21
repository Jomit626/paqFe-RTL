package coder

import chisel3._
import chisel3.util._

import types._


class ArithCoder extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new BitProbBundle))
    val out = ValidIO(new ByteBundle)
  })

  val prob = io.in.bits.prob
  val bit = io.in.bits.bit
  val last = io.in.bits.last

  val low = RegInit(0.U(32.W))
  val high = RegInit(0xFFFFFFFFL.U(32.W))

  val mul = (high - low) * prob
  val mid = low + (mul >> 12)

  val lowUpdate = Mux(bit === 1.U, low, mid + 1.U)
  val highUpdate = Mux(bit === 1.U, mid, high)

  val shift = (lowUpdate(31, 24) === highUpdate(31, 24))

  val lowNxt = Mux(shift, Cat(lowUpdate(23,0), 0.U(8.W)), lowUpdate)
  val highNxt = Mux(shift, Cat(highUpdate(23,0), 0xFF.U(8.W)), highUpdate)

  val outputOH = RegInit(1.U(4.W))
  val outputLast = RegInit(false.B)
  outputLast := (io.in.valid && last) || (outputLast && ~outputOH(3))

  when(outputLast) {
    outputOH := Cat(outputOH(2,0), outputOH(3))
  }
  val tailLast = outputOH(3)
  val tailByte = Mux1H(outputOH, Seq(low(31, 24), low(23,16), low(15,8), low(7,0)))

  when(io.in.valid) {
    low := lowNxt
    high := highNxt
  }.elsewhen(tailLast) {
    low := 0.U
    high := 0xFFFFFFFFL.U
  }

  io.out.bits.byte := Mux(outputLast, tailByte, lowUpdate(31, 24))
  io.out.bits.last := outputOH(3) === 1.U
  io.out.valid := (io.in.valid && shift) || outputLast
}