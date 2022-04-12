package paqFe.models


import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._

import chiseltest._

import paqFe._
import paqFe.verifydata._
import paqFe.types._

class ContextMapSpec extends SpecClass {
  behavior of "ContextMap"
  it should s"work " in {
    test(new ContextMap(16))
    .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
      statusWaitInitDone(c.clock, c.io.status, 1 << 16)
      c.io.in.initSource()
      c.io.in.setSourceClock(c.clock)
      c.io.out.ready.poke(true.B)
      c.io.outHit.ready.poke(true.B)
      fork {
        val bd = new NibbleCtxBundle(16)
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0xE.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0x5.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0x40.U,
          _.last -> false.B,
          _.nibble -> 0x6.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0x9.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0x5.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0x40.U,
          _.last -> false.B,
          _.nibble -> 0x7.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0x5.U
        ))
        c.io.in.enqueue(bd.Lit(
          _.context -> 0x6D8F.U,
          _.chk -> 0xF6.U,
          _.last -> false.B,
          _.nibble -> 0xe.U
        ))
        c.clock.step(5)
      }.join()
    }
  }
}