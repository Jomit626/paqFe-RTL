package paqFe.mixer

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._
import chiseltest.simulator.SimulatorDebugAnnotation

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import Layer1Helpers._

class MixerLayer1Test(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new MixerInputBundle)))

    val out = Vec(8, Vec(p.nHidden, ValidIO(new XBitBundle())))
    val status = Output(new StatusBundle())
  })

  val insts = Seq.tabulate(8){i => Module(new MixerLayer1()) }

  for(i <- 0 until 8) {
    io.in(i) <> insts(i).io.in
    insts(i).io.out <> io.out(i)
  }
  io.status := StatusMerge(insts.map(_.io.status))
}

class MixerLayer1Spec extends SpecClass {
  behavior of "Mixer Layer 1"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer-l1")
  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new MixerLayer1Test())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }
}

object Layer1Helpers {

implicit class Layer1TestDUT(c: MixerLayer1Test)(implicit p: MixerParameter) {
  def init() = {
    for(in <- c.io.in) {
      in.initSource()
      in.setSourceClock(c.clock)
    }
    for(outs <- c.io.out) {
      for(out <- outs) {
        out.initSink()
        out.setSinkClock(c.clock)
      }
    }
    
    statusWaitInitDone(c.clock, c.io.status)
  }

  def test(dbFile: String) = {
    new VerifyDataset(dbFile).forEachBatch(4096) { (data, last) =>

      val xStart = 0
      val xEnd = xStart + p.nFeatures
      val ctxStart = xEnd
      val ctxEnd = ctxStart + p.nHidden
      val outXStart = ctxEnd
      val outXEnd = outXStart + p.nHidden
      val bitIdx = outXEnd

      var threadList = fork {}
      for(mixer_idx <- 0 until 8) {
        assert(data.length % 8 == 0)
        val n = data.length / 8 // n input per mixer
        // feed x ctx
        threadList = threadList.fork {
          val bd = new MixerInputBundle
          for(i <- 0 until n) {
            val line = data(i * 8 + mixer_idx)
            val x = line.slice(xStart, xEnd)
            val ctx = line.slice(ctxStart, ctxEnd)
            val bit = line(bitIdx)

            c.io.in(mixer_idx).enqueue(bd.Lit(
              _.x ->  Vec.Lit(x.map(_.S(p.XWidth)):_*),
              _.ctx ->  Vec.Lit(ctx.map(_.U(8.W)):_*),
              _.bit -> bit.U,
              _.last -> (last && i == n - 1).B
            ))
          }
        } 
        // expect bit X
        for(idx <- 0 until p.nHidden) {
          threadList = threadList.fork {
            val bd = new XBitBundle
            for(i <- 0 until n) {
              val line = data(i * 8 + mixer_idx)
              val bit = line(bitIdx)
              val x = line(outXStart + idx)
              c.io.out(mixer_idx)(idx).expectDequeue(bd.Lit(
                _.bit -> bit.U,
                _.x -> x.S,
                _.last -> (last && i == n - 1).B
              ))
            }
          }
        }
      }

      threadList.join()
    }
  }
}

}
