package testhelpers

import chisel3._
import chiseltest._

import types._

object Helpers {
  def statusWaitInitDone(clock: Clock, status : StatusBundle, timeout : Int = (1 << 12)) {
    clock.setTimeout(1000 + timeout)
    var done = false
    while(!done) {
      clock.step()
      done = status.initDone.peek().litToBoolean
    }
    clock.setTimeout(1000)
  }
}