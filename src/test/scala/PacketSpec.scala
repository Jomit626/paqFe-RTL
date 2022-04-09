package paqFe

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import verifydata._
import types._


class PacketOutputSpec extends AnyFlatSpec
  with ChiselScalatestTester {
  
  behavior of "PacketOutput"
  it should "align output and add end package" in {
    test(new PacketOutput()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
      dut.io.in.initSource()
      dut.io.in.setSourceClock(dut.clock)

      dut.io.out.initSink()
      dut.io.out.setSinkClock(dut.clock)

      dut.clock.step(1)

      val packetSize = dut.PacketSize
      val nTransferPerPacket = packetSize / 2
      val testSizeVector = Vector(nTransferPerPacket, nTransferPerPacket * 2 + nTransferPerPacket / 2, nTransferPerPacket - 1, nTransferPerPacket + 1)
      
      fork {
        for(n <- testSizeVector) {
          val data = generateInputData(n)
          
          dut.io.in.enqueueSeq(data)
        }
      }.fork{
        for(n <- testSizeVector) {
          val data = generateOutputData(n, packetSize)
          dut.io.out.expectDequeueSeq(data)
        }
      }.join()
    }

  }

  def generateInputData(n: Int) = {
    val bd = new ByteIdxBundle
    (0 until n).map{i => 
      bd.Lit(
        _.byte -> (i & 0xFF).U,
        _.idx -> ((i + 1) & 0xFF).U,
        _.last -> (i + 1 >= n).B
      )
    }
  }

  def generateOutputData(n: Int, packetSize: Int) = {
    val bd = new CompressorOutputBundle
    val nTransferPerPacket = packetSize / 2
    val nVaildDataInPacket = nTransferPerPacket - 1
    val totalTransfer = n

    val nullElem = bd.Lit(
        _.data -> 0.U,
        _.last -> false.B
      )
    def tailElem(len: Int) = bd.Lit(
        _.data -> len.U,
        _.last -> false.B
      )
    
    val payload = (0 until n).map{i =>
      bd.Lit(
        _.data -> ((((i + 1) & 0xFF) << 8) | (i & 0xFF)).U,
        _.last -> false.B
      )
    }
    
    val endPacket = (0 until nTransferPerPacket).map{i =>
      bd.Lit(
        _.data -> 0.U,
        _.last -> (i + 1 >= nTransferPerPacket).B
      )
    }

    var data : IndexedSeq[types.CompressorOutputBundle] = null
    for(i <- 0 until(totalTransfer, nVaildDataInPacket)) {

      val packetPayload = payload.slice(i, i + nVaildDataInPacket)
      val packet = packetPayload.padTo(nVaildDataInPacket, nullElem).appended(tailElem(packetPayload.length))
      if(data == null) {
        data = packet
      }else {
        data = data ++ packet
      }
    }

    data ++ endPacket
  }
}
