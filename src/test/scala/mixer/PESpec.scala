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

import Helpers._

class PredictPESpec extends SpecClass{
  behavior of "Mixer Prediction Function"
  val mixerPredictDB = new VerifyData("./paqFe/verify/db/mixer-l1-pe-predict")

  for(line <- mixerPredictDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new PredictPE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()

        c.test(output_file)
      }
    }

  }
}

class LossCalPESpec extends SpecClass {
  behavior of "Loss Calcuation PE"
  val mixerLossDB = new VerifyData("./paqFe/verify/db/mixer-l1-pe-loss")
  for(line <- mixerLossDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)
    
    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new LossCalPE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }
}

class UpdatePESpec extends SpecClass {
  behavior of "Weight Update PE"
  val mixerUpdateDB = new VerifyData("./paqFe/verify/db/mixer-l1-pe-update")
  for(line <- mixerUpdateDB.data) {
    val data_name = line(0)
    val input_file = line(1)
    val output_file = line(2)

    implicit val p = GetMixerConfig()
    it should s"match software model with data: $data_name" in {
      test(new UpdatePE())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.init()
        c.test(output_file)
      }
    }
  }
}

object Helpers {

implicit class LossCalPEDUT(c : LossCalPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.P.initSource()
    c.io.P.setSourceClock(c.clock)

    c.io.loss.initSink()
    c.io.loss.setSinkClock(c.clock)
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
      c.io.loss.expectDequeueNow(loss.S(p.lossWidth))
    }
  }
}

implicit class PredictPEDUT(c : PredictPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.W.initSource()
    c.io.W.setSourceClock(c.clock)

    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.P.initSink()
    c.io.P.setSinkClock(c.clock)

    c.io.updateStrm.initSink()
    c.io.updateStrm.setSinkClock(c.clock)
  }

  def test_inst(data: IndexedSeq[Seq[Int]]) = {
    val nFeatures = p.nFeatures
    fork {
      // feed Xs and bit
      // X(n), W(n), bit, prob
      val sfrom = 0
      val suntil = sfrom + nFeatures
      val bitIdx = nFeatures * 2
      val bd = new PredictUpdateEngineXCtrlBundle()
      for(line <- data) {
        val Xs = line.slice(sfrom, suntil)
        val bit = line(bitIdx)
        c.io.in.enqueue(bd.Lit(
          _.X -> Vec.Lit(Xs.map(_.S(p.XWidth)):_*),
          _.bit -> bit.U,
          _.harzardFastPath -> false.B
        ))
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
      // expect update stream
      c.io.updateStrm.ready.poke(true.B)
      for(line <- data) {
        val X = line.slice(0, nFeatures)
        val W = line.slice(nFeatures, nFeatures * 2)

        for(j <- 0 until p.VecDotII) {
          var valid = c.io.updateStrm.valid.peek().litToBoolean
          while(!valid) {
            c.clock.step()
            valid = c.io.updateStrm.valid.peek().litToBoolean
          }
          
          for(i <- 0 until p.VecDotMACNum) {
            if(j * p.VecDotMACNum + i < nFeatures) {
              c.io.updateStrm.bits.w(i).expect(W(j * p.VecDotMACNum + i).S)
              c.io.updateStrm.bits.x(i).expect(X(j * p.VecDotMACNum + i).S)
            }
          }
          c.clock.step()
        }
      }
    }.join()
  }

  def test(dbFile : String) = {
    val reader = CSVReader.open(new File(dbFile))
    
    val it = reader.iterator
    var end = false
    while(!end) { // TODO: make a util
      val data = it.take(100000).map{ _.map(_.toInt)}.toIndexedSeq
      if(!data.isEmpty) {
        test_inst(data)
      }
      end = data.isEmpty
    }
  }
}

implicit class UpdatePEDUT(c : UpdatePE)(implicit p : MixerParameter) {

  def init() = {
    c.io.loss.initSource()
    c.io.loss.setSourceClock(c.clock)

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

        c.io.loss.enqueueNow(loss.S(p.lossWidth))

      }
    }.fork {
      // expect Wu
      val sfrom = n * 2
      val suntil = sfrom + n
      val bd = new WeightsWriteBackBundle()
      for(line <- data) {
        val data = line.slice(sfrom, suntil)

      }
    }.join()
  }
}

}