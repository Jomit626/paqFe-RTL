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
    val out = Vec(n, DecoupledIO(new NibbleBundle()))
  })

  val inReady = WireInit(false.B)
  val nibble = WireInit(0.U(4.W))
  val last = WireInit(false.B)
  val outValid = WireInit(false.B)

  val s0 :: s1 :: Nil = Enum(2)
  val state = RegInit(s0)

  val allReady = io.out.map(_.ready).reduce(_ && _)
  
  switch(state) {
    is(s0) {
      nibble := io.in.bits.byte(7,4)
      last := io.in.bits.last
      outValid := allReady && io.in.valid

      when(allReady && io.in.valid) {
        state := s1
      }
    }

    is(s1) {
      nibble := io.in.bits.byte(3,0)
      last := io.in.bits.last

      inReady := allReady
      outValid := allReady

      when(allReady && io.in.valid) {
        state := s0
      }
    }
  }

  io.in.ready := inReady

  for (i <- 0 until n) {
    io.out(i).bits.nibble := nibble
    io.out(i).bits.last := last
    io.out(i).valid := outValid
  }
}

class OrdersContext extends Module {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleBundle()))
    val o1Out = DecoupledIO(new NibbleCtxBundle(12))
    val o2Out = DecoupledIO(new NibbleCtxBundle(16))
    val o3Out = DecoupledIO(new NibbleCtxBundle(16))
    val o4Out = DecoupledIO(new NibbleCtxBundle(17))
  })

  val outValid = RegEnable(io.in.valid, false.B, io.in.ready)

  // datapath
  val C = RegInit(0.U(32.W))
  
  val C1 = RegInit(0.U(13.W))
  val C2 = RegInit(0.U(21.W))
  val C3 = RegInit(0.U(29.W))
  val C4 = RegInit(0.U(37.W))

  val h1 = C1
  val h2 = TabHash(C2, O2HashTab())
  val h3 = TabHash(C3, O3HashTab())
  val h4 = TabHash(C4, O4HashTab())

  val nibble = RegEnable(io.in.bits.nibble, 0.U, io.in.ready)
  val last = RegInit(false.B)
  val last_d = RegInit(false.B)

  val outFire = io.o1Out.fire
  val flag = RegInit(false.B)
  val nFirst = RegInit(false.B)

  val CNxt = Cat(C, nibble)
  when(outFire) {
    nFirst := true.B

    C := CNxt
    when(flag) {
      C1 := Cat(CNxt(7, 0), 0.U(5.W))
      C2 := Cat(CNxt(15, 0), 0.U(5.W))
      C3 := Cat(CNxt(23, 0), 0.U(5.W))
      C4 := Cat(CNxt(31, 0), 0.U(5.W))
    } otherwise {
      C1 := Cat(C1(12, 5), 1.U(1.W), nibble)
      C2 := Cat(C2(20, 5), 1.U(1.W), nibble)
      C3 := Cat(C3(28, 5), 1.U(1.W), nibble)
      C4 := Cat(C4(36, 5), 1.U(1.W), nibble)
    }
    flag := ~flag
  }

  when(io.in.fire) {
    last := io.in.bits.last
    last_d := last
  }

  when(outFire && last && last_d) {
    nFirst := false.B
    nibble := 0.U
    C := 0.U
    C1 := 0.U
    C2 := 0.U
    C3 := 0.U
    C4 := 0.U
  }

  val allReady = Seq(io.o1Out, io.o2Out, io.o3Out, io.o4Out).map(_.ready).reduce(_ && _)
  io.in.ready := allReady

  io.o1Out.bits.context := Mux(nFirst, h1, 0.U)
  io.o1Out.bits.chk := C1(7,0)
  io.o1Out.bits.last := last
  io.o1Out.bits.nibble := nibble
  io.o1Out.valid := outValid & allReady

  io.o2Out.bits.context := Mux(nFirst, h2, 0.U)
  io.o2Out.bits.chk := C2(7,0)
  io.o2Out.bits.last := last
  io.o2Out.bits.nibble := nibble
  io.o2Out.valid := outValid & allReady

  io.o3Out.bits.context := Mux(nFirst, h3, 0.U)
  io.o3Out.bits.chk := C3(7,0)
  io.o3Out.bits.last := last
  io.o3Out.bits.nibble := nibble
  io.o3Out.valid := outValid & allReady

  io.o4Out.bits.context := Mux(nFirst, h4, 0.U)
  io.o4Out.bits.chk := C4(7,0)
  io.o4Out.bits.last := last
  io.o4Out.bits.nibble := nibble
  io.o4Out.valid := outValid & allReady
}

class Orders extends Module {
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(new NibbleBundle()))

    val outProb = Vec(4, Vec(8, DecoupledIO(new BitProbBundle())))
    val outCtx = Vec(8, DecoupledIO(UInt(3.W)))

    val status = new StatusBundle
  })

  val contextGen = Module(new OrdersContext())
  
  val o1CtxMap = Module(new ContextMap(12))
  val o2CtxMap = Module(new ContextMap(16))
  val o3CtxMap = Module(new ContextMap(16))
  val o4CtxMap = Module(new ContextMap(17))
  val contextMaps = Seq(o1CtxMap, o2CtxMap, o3CtxMap, o4CtxMap)

  val convter = Module(new ContextMapsToModel(4))

  io.in <> contextGen.io.in
  o1CtxMap.io.in <> contextGen.io.o1Out
  o2CtxMap.io.in <> contextGen.io.o2Out
  o3CtxMap.io.in <> contextGen.io.o3Out
  o4CtxMap.io.in <> contextGen.io.o4Out

  convter.io.in(0) <> o1CtxMap.io.out
  convter.io.in(1) <> o2CtxMap.io.out
  convter.io.in(2) <> o3CtxMap.io.out
  convter.io.in(3) <> o4CtxMap.io.out

  io.outProb <> convter.io.out
  io.outCtx <> convter.io.outCtx
  
  io.status := RegNext(StatusMerge(contextMaps.map(_.io.status)))
}
