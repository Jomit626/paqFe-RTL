package paqFe.mixer

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._
import chiseltest.simulator.SimulatorDebugAnnotation

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import Layer1Helpers._

class MixerLayer1Spec extends SpecClass {
  behavior of "Mixer Layer 1"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer-l1")
  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new MixerLayer1())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }
}

object Layer1Helpers {

implicit class Layer1TestDUT(c: MixerLayer1)(implicit p: MixerParameter) {
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.out.foreach { out =>
      out.initSink()
      out.setSinkClock(c.clock)
    }
    
    statusWaitInitDone(c.clock, c.io.status)
  }

  def test(dbFile: String) = {
    new VerifyDataset(dbFile).forEachBatch(1024) { (data, last) =>

      val xStart = 0
      val xEnd = xStart + p.nFeatures
      val ctxStart = xEnd
      val ctxEnd = ctxStart + p.nHidden
      val outXStart = ctxEnd
      val outXEnd = outXStart + p.nHidden
      val bitIdx = outXEnd

      // feed x ctx
      var forkList = fork {
        val bd = new MixerInputBundle
        for((line, i) <- data.zipWithIndex) {
          val x = line.slice(xStart, xEnd)
          val ctx = line.slice(ctxStart, ctxEnd)
          val bit = line(bitIdx)
          c.io.in.enqueue(bd.Lit(
            _.x ->  Vec.Lit(x.map(_.S(p.XWidth)):_*),
            _.ctx ->  Vec.Lit(ctx.map(_.U(8.W)):_*),
            _.bit -> bit.U,
            _.last -> (last && i == data.length - 1).B
          ))
        }
      }

      // expect bit X
      for(idx <- 0 until p.nHidden) {
        forkList = forkList.fork {
          val bd = new XBitBundle
          for((line, i) <- data.zipWithIndex) {
            val bit = line(bitIdx)
            val x = line(outXStart + idx)
            c.io.out(idx).expectDequeue(bd.Lit(
              _.bit -> bit.U,
              _.x -> x.S,
              _.last -> (last && i == data.length - 1).B
            ))
          }
        }
      }

      forkList.join()
    }
  }
}

}
