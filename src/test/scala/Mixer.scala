import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import verifydata._
import types._

import java.io._
import com.github.tototoshi.csv._
import chisel3.experimental.VecLiterals._

import LocalHelpers._
import testhelpers.Helpers._

import Helpers._

class MixerSubModuleSpec
  extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {
  
  assert(TreeReduce[Int](Seq(1,2,3,4), _ + _) == 10)
  assert(TreeReduce[Int](Seq(1,2,3,4,5), _ + _) == 15)

  behavior of "Mixer Prediction Function"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer-predict")
  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    val testVectorLen = CSVReader.open(new File(output_file)).readNext().get.size
    // X(n), W(n), bit, prob
    assert((testVectorLen - 2) % 2 == 0)
    val nFeatures = (testVectorLen - 2) / 2

    implicit val p = new MixerParameter(nFeatures)
    it should s"match software model with data: $data_name" in {
      test(new PredictPE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()

        c.test(output_file)
      }
    }

  }

  behavior of "Loss Calcuation"
  val mixerLossDB = new VerifyData("./paqFe/verify/db/mixer-loss")
  for(line <- mixerLossDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    implicit val p = new MixerParameter()
    it should s"match software model with data: $data_name" in {
      test(new LossCalPE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }

  behavior of "Weight Update"
  val mixerUpdateDB = new VerifyData("./paqFe/verify/db/mixer-update")
  for(line <- mixerUpdateDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    val testVectorLen = CSVReader.open(new File(output_file)).readNext().get.size
    // X(n), W(n),  Wu(n), loss
    assert((testVectorLen - 1) % 3 == 0)
    val nFeatures = (testVectorLen - 1) / 3

    implicit val p = new MixerParameter(nFeatures)
    it should s"match software model with data: $data_name" in {
      test(new UpdatePE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }
  /*
  behavior of "Mixer"

  val db = new VerifyData("./paqFe/verify/db/mixer")
  for(line <- db.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    it should s"match software model with data: $data_name" in {
      test(new Mixer(5))
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      
        MixerDUT.init(c)
        statusWaitInitDone(c.clock, c.io.status)

        MixerDUT.test(c, output_file)
      }
    }
  }
  */
}
object Helpers {

implicit class LossCalPEDUT(c : LossCalPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.P.initSource()
    c.io.P.setSourceClock(c.clock)
  }

  def test( dbFile : String) = {
    val reader = CSVReader.open(new File(dbFile))
    val data = reader.all().map{ _.map(_.toInt)}.toIndexedSeq

    val bd = new BitProbBundle()
    for(line <- data) {
      val bit = line(0)
      val prob = line(1)
      val loss = line(2)

      c.io.P.enqueueNow(bd.Lit(_.bit -> bit.U, _.prob -> prob.U, _.last -> false.B))
      c.clock.step()
      c.io.loss.expect(loss.S(p.lossWidth))
    }
  }
}

implicit class PredictPEDUT(c : PredictPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.W.initSource()
    c.io.W.setSourceClock(c.clock)

    c.io.X.initSource()
    c.io.X.setSourceClock(c.clock)

    c.io.ctrl.initSource()
    c.io.ctrl.setSourceClock(c.clock)

    c.io.P.initSink()
    c.io.P.setSinkClock(c.clock)

    c.io.updateStrm.initSink()
    c.io.updateStrm.setSinkClock(c.clock)
  }

  def test(dbFile : String) = {
    val reader = CSVReader.open(new File(dbFile))
    val data = reader.all().map{ _.map(_.toInt)}.toIndexedSeq
    val nFeatures = p.nFeatures

    fork {
      // feed X
      // X(n), W(n), bit, prob
      val sfrom = 0
      val suntil = sfrom + nFeatures
      for(line <- data) {
        val data = line.slice(sfrom, suntil)
        assert(data.length == nFeatures)
        c.io.X.enqueue(Vec.Lit(data.map(_.S(p.XWidth)):_*))
      }
    }.fork {
      // feed W
      // X(n), W(n), bit, prob
      val sfrom = nFeatures
      val suntil = sfrom + nFeatures
      for(line <- data) {
        val data = line.slice(sfrom, suntil)
        c.io.W.enqueue(Vec.Lit(data.map(_.S(p.WeightWidth)):_*))
      }
    }.fork {
      // feed bit
      val idx = nFeatures * 2
      for(line <- data) {
        val data = line(idx)
        c.io.ctrl.enqueue(data.U)
      }
    }.fork {
      // expect P
      val bIdx = nFeatures * 2
      val pIdx = nFeatures * 2 + 1
      val db = new BitProbBundle()
      for(line <- data) {
        val bit = line(bIdx)
        val prob = line(pIdx)
        c.io.P.expectDequeue(db.Lit(_.bit -> bit.U, _.prob -> prob.U, _.last -> false.B))
      }
    }.fork {
      // expect stream, focus on timming
      for(line <- data) {
        val X = line.slice(0, nFeatures)
        val W = line.slice(nFeatures, nFeatures * 2)

        var valid = c.io.updateStrm.valid.peek().litToBoolean
        while(!valid) {
          c.clock.step()
          valid = c.io.updateStrm.valid.peek().litToBoolean
        }

        for(i <- 0 until nFeatures) {
          c.io.updateStrm.valid.expect(true.B)
          c.io.updateStrm.bits.w.expect(W(i).S(p.WeightWidth))
          c.io.updateStrm.bits.x.expect(X(i).S(p.XWidth))
          c.clock.step()
        }
      }
    }.join()

  }
}

implicit class UpdatePEDUT(c : UpdatePE)(implicit p : MixerParameter) {

  def init() = {
    c.io.updateStrm.initSource()
    c.io.updateStrm.setSourceClock(c.clock)

    c.io.wStrm.initSink()
    c.io.wStrm.setSinkClock(c.clock)
  }

  def test(dbFile : String) = {
    val reader = CSVReader.open(new File(dbFile))
    val data = reader.all().map{ _.map(_.toInt)}.toIndexedSeq
    val n = p.nFeatures
    // line
    // X(n), W(n), Wu(n), loss
    fork {
      // feed X, W, loss
      val xfrom = 0
      val xuntil = xfrom + n
      val wfrom = n
      val wuntil = wfrom + n
      val lossIdx = n * 3
      for(line <- data) {
        val X = line.slice(xfrom, xuntil)
        val W = line.slice(wfrom, wuntil)
        val loss = line(lossIdx)

        c.io.loss.poke(loss.S(p.lossWidth))
        for(i <- 0 until n) {
          c.io.updateStrm.valid.poke(true.B)
          c.io.updateStrm.bits.w.poke(W(i).S(p.WeightWidth))
          c.io.updateStrm.bits.x.poke(X(i).S(p.XWidth))
          c.clock.step()
        }
        c.io.updateStrm.valid.poke(false.B)
      }
    }.fork {
      // expect Wu
      val sfrom = n * 2
      val suntil = sfrom + n
      for(line <- data) {
        val data = line.slice(sfrom, suntil)
        for(w <- data) {
          c.io.wStrm.expectDequeue(w.S(p.WeightWidth))
        }
      }
    }.join()
  }
}

private object MixerDUT {
  def init(c : Mixer) = {
    for(i <- 0 until 8) {
      c.io.in(i).initSource()
      c.io.in(i).setSourceClock(c.clock)
      c.io.out(i).initSink()
      c.io.out(i).setSinkClock(c.clock)
      c.io.in(i).valid.poke(false.B)
    }

    c.reset.poke(true.B)
    c.clock.step(16)
    c.reset.poke(false.B)
    c.clock.step()
  }

  def test(c : Mixer, db_file : String) = {
    val reader = CSVReader.open(new File(db_file))

    val data = reader.all().map{ _.map(_.toInt)}.toIndexedSeq
    assert(data.length % 8 == 0)

    var thr = fork {}
    for(i <- 0 until 8) {
      thr = thr.fork {
        val exp = new BitProbBundle()
        for(idx <- 0 until (data.length / 8)) {
          val t = data(idx * 8 + i).takeRight(2)
          val bit = t(0)
          val prob = t(1)

          c.io.out(i).expectDequeue(exp.Lit(
            _.bit -> bit.U,
            _.prob -> prob.U,
            _.last -> (idx == (data.length / 8 - 1)).B
          ))
        }
      }

      thr = thr.fork {
        val in = new BitProbsCtxBundle(c.nProb)
        for(idx <- 0 until (data.length / 8)) {
          assert(data(idx * 8 + i)(0) == i)
          val t = data(idx * 8 + i).drop(1)
          val probs = t.take(c.nProb)
          val ctx = t(c.nProb)
          val bit = t(c.nProb + 1)

          c.io.in(i).enqueue(in.Lit(
            _.probs -> Vec.Lit(probs.map(_.U(12.W)):_*),
            _.ctx -> ctx.U,
            _.bit -> bit.U,
            _.last -> (idx == (data.length / 8 - 1)).B
          ))
        }
      }
    }
    
    thr.join()
  }
}

}