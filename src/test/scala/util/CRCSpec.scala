package paqFe.util

import org.scalatest._

import chisel3._
import chisel3.util._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import paqFe.verifydata._

class CRCSpec extends AnyFlatSpec
  with ChiselScalatestTester{

  val db = new VerifyData("./verify/db/crc")
  
  def readCRCOutputFile(filename: String) : Long = {
    val line = Source.fromFile(filename).mkString.stripLineEnd
    line.toLong
  }

  behavior of "CRC Table"
  val crc = new CRC(32)

  for(line <- db.data) {
    val test_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should s"Match CPP impl with data $test_name" in {
      test(new CRCModule(32)) { dut =>
        val fin = new ByteStream(input_file)
        val checkSumTest = readCRCOutputFile(output_file)
        
        var checkSum = 0L
        dut.io.reload.poke(false.B)

        var last = false
        while(!last) {
          val (b, l) = fin.getByte()
          last = l

          dut.io.in.bits.poke(b.U)
          dut.io.in.valid.poke(true.B)
          dut.clock.step()
          dut.io.in.valid.poke(false.B)

          checkSum = crc.next(checkSum, b)
        }

        dut.io.out.expect(checkSumTest.U(32.W))
        assert(checkSum == checkSumTest)
      }
    }
  }
}

