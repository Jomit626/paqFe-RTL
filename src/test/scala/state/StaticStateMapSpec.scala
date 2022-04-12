package paqFe.state

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import paqFe.verifydata._

class StaticStateMapTest extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(8.W))

    val p = Output(UInt(12.W))
  })

  io.p := StaticStateMap(io.state)
}

class StaticStateMapSpec extends AnyFlatSpec
  with ChiselScalatestTester {
  behavior of "State Shift"
  it should "match software" in {
    test(new StaticStateMapTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      val db = new VerifyData("./paqFe/verify/db/staticstatemap")
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        new VerifyDataset(output_file).forAll { data =>
          for((line, i) <- data.zipWithIndex) {
            c.io.state.poke(i.U)
            c.io.p.expect(line(0).U)
          }
        }
      }
    }
  }
}
  