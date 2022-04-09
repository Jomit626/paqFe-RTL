import chisel3._
import chisel3.util._

import types.{ByteIdxBundle,CompressorOutputBundle}

class PacketOutput extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new ByteIdxBundle()))

    val out = DecoupledIO(new CompressorOutputBundle())
  })

  val inReady = WireInit(false.B)
  val outValid = WireInit(false.B)
  val outData = WireInit(0.U)
  val outLast = WireInit(false.B)

  val PacketSize = 4096 // Bytes
  require(isPow2(PacketSize))
  
  val n = PacketSize / (16 / 8) // n input per packer

  val cntInc = WireInit(false.B)
  val cntRst = WireInit(false.B)
  val cnt = RegInit(0.U(log2Ceil(n + 1).W))
  val cntNext =  Mux(cntRst, 0.U, cnt +& cntInc)
  cnt := cntNext

  val len = Reg(UInt())
  val last = RegInit(false.B)
  val lastValidDataInPacket = cnt === (n - 2).U
  
  val sPassthrought :: sAlign :: sOutLen :: Nil = Enum(3)
  val state = RegInit(sPassthrought)

  switch(state) {
    is(sPassthrought) {
      inReady := io.out.ready
      outValid := io.in.valid
      outData := Cat(io.in.bits.idx, io.in.bits.byte)
      cntInc := io.out.fire

      when(io.out.fire) {
        when(lastValidDataInPacket) {
          len := cntNext
          last := io.in.bits.last
          state := sOutLen
        }.elsewhen(io.in.bits.last) {
          len := cntNext
          last := io.in.bits.last
          state := sAlign
        }
      }
    }

    is(sAlign) {
      outValid := true.B
      outData := 0.U
      cntInc := io.out.fire

      when(io.out.fire && lastValidDataInPacket) {
        state := sOutLen
      }
    }

    is(sOutLen) {
      outValid := true.B
      outData := len
      cntRst := io.out.fire
      outLast := len === 0.U

      when(io.out.fire) {
        when(last) {
          state := sAlign
          len := 0.U
        } otherwise {
          state := sPassthrought
        }
        last := false.B
      }
    }
  }

  io.in.ready := inReady
  io.out.bits.data := outData
  io.out.bits.last := outLast
  io.out.valid := outValid
}

class AXISBundle1(Width: Int) extends Bundle {
  require(Width % 8 == 0)

  val TDATA = UInt(Width.W)
  val TLAST = Bool()
}

class PacketInput extends Module {
  val AXIBitWidth = 64
  val AXIByteWidth = AXIBitWidth / 8

  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new AXISBundle1(64)))

    val out = DecoupledIO(new AXISBundle1(64))
  })

  val PacketSize = 1024 // Bytes
  val n = PacketSize / AXIByteWidth // n input per packer

  val packetHead = Reg(UInt(64.W))
  val packetLen = packetHead(0, 55)
  val packetFlag = packetHead(56, 63)


}