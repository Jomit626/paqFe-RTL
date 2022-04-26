package paqFe.util

import chisel3._
import chisel3.util.Valid

class CRCModule(width: Int) extends Module {
  val poly = CRC.getPoly(width)
  val table = CRC.genTable(width)
  require(poly != 0)
  val lut = VecInit(table.map(_.U))

  val io = IO(new Bundle {
    val in = Flipped(Valid(UInt(8.W)))
    val reload = Input(Bool())

    val out = Output(UInt(width.W))
  })

  val crc = RegInit(0.U(width.W))

  val t = lut(crc(31, 24) ^ io.in.bits)
  val crcNext = (crc << 8) ^ t

  when(io.reload) {
    crc := 0.U
  }.elsewhen(io.in.fire) {
    crc := crcNext
  }

  io.out := crc
}

class CRC(width: Int) {
  val poly = CRC.getPoly(width)
  val table = CRC.genTable(width)
  val mask = ((1L << width) - 1)
  require(poly != 0)

  
  def next(crc: Long, byte: Int) = {
    val t = table(((crc >> 24) & 0xFF).toInt ^ (byte & 0xFF))
    ((crc << 8) ^ t) & mask
  }
}

object CRC extends {
  def getPoly(width: Int) = width match {
    case 32 => 0x04C11DB7L
    case 16 => 0x1021L
    case _ => 0
  }

  def genTable(width: Int) = {
    val poly = getPoly(width)

    Seq.tabulate(256) { idx =>
      val top = 1L << (width - 1)
      var reg = idx.toLong << (width - 8)

      for(i <- 0 until 8) {
        if((reg & top) != 0) {
          reg = (reg << 1) ^ poly
        } else {
          reg = (reg << 1)
        }
      }

      reg & ((1L << width) - 1)
    }
  }
}