package paqFe

import chisel3._
import chiseltest._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import paqFe.types._

abstract class SpecClass
  extends AnyFlatSpec
  with ChiselScalatestTester
  with Matchers {
  
}

object statusWaitInitDone {
  def apply(clock: Clock, status : StatusBundle, timeout : Int = (1 << 12), step: Int = 16) = {
    clock.setTimeout(1000 + timeout)

    var done = false
    while(!done) {
      clock.step(step)
      done = status.initDone.peek().litToBoolean
    }
    clock.setTimeout(1000)
  }
}