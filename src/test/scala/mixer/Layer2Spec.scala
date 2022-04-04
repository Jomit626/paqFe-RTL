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

import Layer2Helpers._
import chisel3.experimental.VecLiterals

class MixerLayer2TestModule(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Vec(8, Flipped(DecoupledIO(new Layer2InputBundle)))

    val out = Vec(8,ValidIO(new BitProbBundle()))
  })

  for(i <- 0 until 8) {
    val m = Module(new MixerLayer2)
    io.in(i) <> m.io.in
    io.out(i) := m.io.out
  }
}

class MixerLayer2Spec extends SpecClass {
  behavior of "Mixer Layer 2"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer-l2")
  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new MixerLayer2TestModule())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
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

  def test(dbFile: String) = {
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
              _.last -> false.B
            ))
          }
        }
      }

      forkList.join()
    }
  }
}

}