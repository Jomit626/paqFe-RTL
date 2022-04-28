package paqFe.mixer

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import paqFe.verifydata._
import paqFe.types._

import java.io._
import com.github.tototoshi.csv._
import chisel3.experimental.VecLiterals._

import paqFe._
import scala.util.Random

import Layer2Helpers._

class MixerLayer2TestModule(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new Layer2InputBundle)))

    val out = Vec(8, DecoupledIO(new BitProbBundle()))
  })

  for(i <- 0 until 8) {
    val m = Module(new MixerLayer2)
    io.in(i) <> m.io.in
    io.out(i) <> m.io.out
  }
}

class MixerLayer2Spec extends SpecClass {
  behavior of "Mixer Layer 2"
  val mixerPredictDB = new VerifyData("./verify/db/mixer-l2")
  implicit val p = GetMixerConfig()

  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    it should s"match software model with data: $data_name" in {
      test(new MixerLayer2TestModule())
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }

    it should s"tolerate throttling with data: $data_name" in {
      test(new MixerLayer2TestModule())
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        c.init()
        c.test(output_file, true)
      }
    }
  }
  it should "take multiple streams without reset between and output currect data" in {
    test(new MixerLayer2TestModule())
    .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>

      c.init()
      for(line <- mixerPredictDB.data) {
        val data_name = line(0)
        val input_file = line(1)
        val output_file = line(2)
        
        c.test(output_file, true)
      }
    }
  }
}

object Layer2Helpers {

implicit class Layer2TestDUT(c: MixerLayer2TestModule)(implicit p: MixerParameter) {
  def init() = {
    c.io.in.foreach { in =>
      in.initSource()
      in.setSourceClock(c.clock)
    }

    c.io.out.foreach { out =>
      out.initSink()
      out.setSinkClock(c.clock)
    }
  }

  def test(dbFile: String, throttle: Boolean = false) = {
    new VerifyDataset(dbFile).forAll { data =>
      assert(data.length % 8 == 0)
      val n = data.length / 8

      // feed x
      var forkList = fork { }
      for(idx <- 0 until 8) {
        forkList = forkList.fork {
          val bd = new Layer2InputBundle
          for(i <- 0 until n) {
            val line = data(i * 8 + idx)
            assert(line(0) == idx)
            val x = line.slice(1, 1 + p.nHidden)
            val bit = line(1 + p.nHidden * 2)

            c.io.in(idx).enqueue(bd.Lit(
              _.bit -> bit.U,
              _.last -> (i == n - 1).B,
              _.x -> Vec.Lit(x.map(_.S(p.XWidth)):_*)
            ))
          }
        }
      }

      // expect bit prob
      for(idx <- 0 until 8) {
        forkList = forkList.fork {
          val bd = new BitProbBundle
          for(i <- 0 until n) {
            val line = data(i * 8 + idx)

            val prob = line(1 + p.nHidden * 2 + 1)
            val bit = line(1 + p.nHidden * 2)

            c.io.out(idx).expectDequeue(bd.Lit(
              _.bit -> bit.U,
              _.prob -> prob.U,
              _.last -> (i == n - 1).B,
            ))

            if(throttle) {
              c.clock.step(4)
            }
          }
        }
      }

      forkList.join()
    }
  }
}

}