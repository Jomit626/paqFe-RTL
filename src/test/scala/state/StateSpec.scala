package paqFe.state

import org.scalatest._

import chisel3._
import chisel3.experimental.BundleLiterals._

import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import paqFe.verifydata._

class StateTest extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(8.W))
    val bit = Input(UInt(1.W))

    val next = Output(UInt(8.W))
  })

  io.next := StateShiftLut(io.state, io.bit)
}

class StateSpec extends AnyFlatSpec
  with ChiselScalatestTester {
  behavior of "State Shift"
  it should "match software" in {
    test(new StateTest())
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      val db = new VerifyData("./verify/db/stateshift")
      for(line <- db.data) {
        val test_name = line(0)
        val input_file = line(1)
        val output_file = line(2)

        new VerifyDataset(output_file).forAll { data =>
          for((line, i) <- data.zipWithIndex) {
            c.io.state.poke(i.U)
            c.io.bit.poke(0.U)
            c.io.next.expect(line(0).U)

            c.io.bit.poke(1.U)
            c.io.next.expect(line(1).U)
          }
        }
      }
    }
  }
}
  