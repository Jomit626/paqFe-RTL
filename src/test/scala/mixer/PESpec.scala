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

import PEHelpers._

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

object PEHelpers {

implicit class LossCalPEDUT(c : LossCalPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.P.initSource()
    c.io.P.setSourceClock(c.clock)

    c.io.loss.initSink()
    c.io.loss.setSinkClock(c.clock)
  }

  def test(dbFile : String) = {
    val bd = new BitProbBundle()
    new VerifyDataset(dbFile).forEachBatch(10000) { (data, last) =>
      for(line <- data) {
        val bit = line(0)
        val prob = line(1)
        val loss = line(2)
  
        c.io.P.enqueueNow(bd.Lit(_.bit -> bit.U, _.prob -> prob.U, _.last -> false.B))
        c.io.loss.expectPeek(loss.S(p.lossWidth))
      }
    }
  }
}

implicit class PredictPEDUT(c : PredictPE)(implicit p : MixerParameter) {
  def init() = {
    c.io.in.initSource()
    c.io.in.setSourceClock(c.clock)

    c.io.P.initSink()
    c.io.P.setSinkClock(c.clock)

    c.io.x.initSink()
    c.io.x.setSinkClock(c.clock)

    c.io.updateStrm.initSink()
    c.io.updateStrm.setSinkClock(c.clock)
  }
  
  def test(dbFile : String) = {
    new VerifyDataset(dbFile).forEachBatch(10000) { (data, last) =>
      val nFeatures = p.nFeatures
      fork {
        // feed W, X and bit
        // X(n), W(n), bit, prob
        val xStart = 0
        val xEnd = xStart + nFeatures
        val wStart = nFeatures
        val wEnd = wStart + nFeatures
        val bitIdx = nFeatures * 2
        val bd = new VecDotPackBundle()
        for(line <- data) {
          val x = line.slice(xStart, xEnd)
          val w = line.slice(wStart, wEnd)
          val bit = line(bitIdx)
          c.io.in.enqueue(bd.Lit(
            _.w -> Vec.Lit(w.map(_.S(p.WeightWidth)):_*),
            _.x -> Vec.Lit(x.map(_.S(p.XWidth)):_*),
            _.bit -> bit.U,
          ))
        }
      }.fork {
        // expect P
        val bIdx = nFeatures * 2
        val xIdx = nFeatures * 2 + 1
        val db = XBitBundle()
        for(line <- data) {
          val bit = line(bIdx)
          val x = line(xIdx)
          c.io.x.expectDequeue(db.Lit(_.bit -> bit.U, _.x -> x.S))
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
  }
}

implicit class UpdatePEDUT(c : UpdatePE)(implicit p : MixerParameter) {

  def init() = {
    c.io.loss.initSource()
    c.io.loss.setSourceClock(c.clock)

    //c.io.updateStrm.initSource()
    //c.io.updateStrm.setSourceClock(c.clock)

    c.io.wStrm.initSink()
    c.io.wStrm.setSinkClock(c.clock)
  }

  def test(dbFile : String) = {
    val n = p.nFeatures
    new VerifyDataset(dbFile).forEachBatch(2000) { (data, last) =>
      // line
      // X(n), W(n), Wu(n), loss
      fork {
        // feed X, W
        val xfrom = 0
        val xuntil = xfrom + n
        val wfrom = n
        val wuntil = wfrom + n
        
        for(line <- data) {
          val X = line.slice(xfrom, xuntil)
          val W = line.slice(wfrom, wuntil)
          for(j <- 0 until p.VecDotII) {
            while(!c.io.updateStrm.ready.peek().litToBoolean)
              c.clock.step()
            
            for(i <- 0 until p.VecDotMACNum) {
              if(j * p.VecDotMACNum + i < p.nFeatures) {
                c.io.updateStrm.bits.w(i).poke(W(j * p.VecDotMACNum + i).S)
                c.io.updateStrm.bits.x(i).poke(X(j * p.VecDotMACNum + i).S)
              } else {
                c.io.updateStrm.bits.w(i).poke(0.S)
                c.io.updateStrm.bits.x(i).poke(0.S)
              }
            }

            c.io.updateStrm.valid.poke(true.B)
            c.clock.step()
          }
        }
      }.fork {
        // feed loss
        val lossIdx = n * 3
        c.clock.step(p.VecDotII)
        for(line <- data) {
          val loss = line(lossIdx)
          c.io.loss.enqueueNow(loss.S)
          c.clock.step(p.VecDotII + 1)
        }
      }.fork {
        // expect Wu
        val sfrom = n * 2
        val suntil = sfrom + n
        val bd = new WeightsWriteBackBundle()
        for(line <- data) {
          val Wu = line.slice(sfrom, suntil)

          for(j <- 0 until p.VecDotII) {
            c.io.wStrm.waitForValid()

            for(i <- 0 until p.VecDotMACNum) {
              if(j * p.VecDotMACNum + i < p.nFeatures) {
                c.io.wStrm.bits.w(i).expect(Wu(j * p.VecDotMACNum + i).S)
              }
            }
            c.clock.step()
          }
        }
      }.join()
    }
  }
}

}