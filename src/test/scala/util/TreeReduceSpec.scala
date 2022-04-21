package paqFe.util

import chisel3._
import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import paqFe.SpecClass
import chisel3.util.log2Ceil

class TreeReduceTest(n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(n, Input(UInt(32.W)))

    val out = Output(UInt(32.W))
  })
  def sum(a: UInt, b: UInt) = a +& b
  def layerOp(i: Int, x: UInt) = if(i % 2 == 0) RegNext(x) else x

  io.out := TreeReduce[UInt](io.in, sum, layerOp(_,_))
}

class TreeReduceSpec extends SpecClass {
  behavior of "TreeReduce"
  it should "Do reducing" in {
    def add(a: Int, b: Int) = a + b
    def testData(data: Seq[Int]) = assert(TreeReduce(data, add) == data.reduce(add))

    testData(Seq(1, 2, 3, 4))
    testData(Seq(1, 2, 3, 4, 5))
  }

  it should "Do proper layer operataion" in {
    test(new TreeReduceTest(64)) { dut =>
      val latency = log2Ceil(64) / 2

      def pokeZeros() = {
        for(in <- dut.io.in) {
          in.poke(0.U)
        }
      }

      def poke(data: Iterable[Int]) = {
        dut.io.in zip data foreach { case (in, x) =>
          in.poke(x.U)
        }
      }

      val data = 0 until 64

      pokeZeros()
      dut.clock.step()
      poke(data)
      dut.clock.step()
      pokeZeros()
      dut.clock.step(latency - 2)
      dut.io.out.expect(0.U)
      dut.clock.step()
      dut.io.out.expect(data.reduce(_ + _).U)
      dut.clock.step()
      dut.io.out.expect(0.U)
    }
  }
}
