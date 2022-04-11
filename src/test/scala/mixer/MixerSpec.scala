package paqFe.mixer

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._
import org.scalatest._
import scala.util.Random

import paqFe._
import paqFe.verifydata._
import paqFe.types._

import MixerSpecHelpers._

class MixerSpec extends SpecClass {
  behavior of "Mixer"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer")
  implicit val p = GetMixerConfig()

  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should s"match software model with data: $data_name" in {
      test(new Mixer())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
    it should s"tolerate throttling with data: $data_name" in {
      test(new Mixer())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file, true)
      }
    }
  }

  it should "take multiple streams without reset between and output currect data" in {
    test(new Mixer())
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

implicit class MixerDUT(c: Mixer)(implicit p: MixerParameter) {
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.out.initSink()
    c.io.out.setSinkClock(c.clock)

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
            _.last -> (i == data.length - 1).B
          ))
        }
      }.fork { // expect prob
        val bd = new BitProbBundle
        for((line, i) <- data.zipWithIndex) {
          val prob = line(probIdx)
          val bit = line(bitIdx)
          c.io.out.expectDequeue(bd.Lit(
            _.prob ->  prob.U,
            _.bit -> bit.U,
            _.last -> (i == data.length - 1).B
          ))

          if(throttle) {
            c.clock.step(Random.between(0, 128))
          }
        }
      }.join()
    }
  }
}

}
