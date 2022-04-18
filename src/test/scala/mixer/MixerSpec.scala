package paqFe.mixer

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._
import org.scalatest._
import scala.util.Random

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import MixerSpecHelpers._

class MixerTest(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new MixerInputBundle)))

    val out = Vec(8, DecoupledIO(new BitProbBundle()))
    val status = Output(new StatusBundle)
  })

  val insts = Seq.tabulate(8){i => Module(new Mixer(i == 0)) }

  for(i <- 0 until 8) {
    io.in(i) <> insts(i).io.in
    insts(i).io.out <> io.out(i)
  }
  io.status := StatusMerge(insts.map(_.io.status))
}

class MixerSpec extends SpecClass {
  behavior of "Mixer"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer")
  implicit val p = GetMixerConfig()

  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should s"match software model with data: $data_name" in {
      test(new MixerTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
    
    it should s"tolerate throttling with data: $data_name" in {
      test(new MixerTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file, true)
      }
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new MixerTest())
    .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>

      c.init()
      for(line <- mixerPredictDB.data) {
        val data_name = line(0)
        val input_file = line(1)
        val output_file = line(2)
        
        c.test(output_file)
        statusWaitInitDone(c.clock, c.io.status)
      }
    }
  }
}

object MixerSpecHelpers {

implicit class MixerDUT(c: MixerTest)(implicit p: MixerParameter) {
  def init() = {
    for(in <- c.io.in) {
      in.initSource()
      in.setSourceClock(c.clock)
    }

    for(out <- c.io.out) {
      out.initSink()
      out.setSinkClock(c.clock)
    }

    statusWaitInitDone(c.clock, c.io.status)
  }

  def test(file: String, throttle: Boolean = false) = {
    new VerifyDataset(file).forAll { data =>
      val xStart = 0
      val xEnd = xStart + p.nFeatures
      val ctxStart = xEnd
      val ctxEnd = ctxStart + p.nHidden
      val bitIdx = ctxEnd
      val probIdx = bitIdx + 1

      var threadList = fork {}
      for(mixer_idx <- 0 until 8) {
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
              _.last -> (i == n - 1).B
            ))
          }
        } fork {  // expect prob
          val bd = new BitProbBundle
          for(i <- 0 until n) {
            val line = data(i * 8 + mixer_idx)
            val prob = line(probIdx)
            val bit = line(bitIdx)
            c.io.out(mixer_idx).expectDequeue(bd.Lit(
              _.prob ->  prob.U,
              _.bit -> bit.U,
              _.last -> (i == n - 1).B
            ))

            if(throttle) {
              c.clock.step(Random.between(0, 32))
            }
          }
        }
      }

      threadList.join()
    }
  }
}

}
