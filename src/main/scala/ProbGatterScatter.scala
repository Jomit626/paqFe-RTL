package paqFe

import chisel3._
import chisel3.util._

import paqFe.types._
import paqFe.util._
import paqFe.mixer.{MixerParameter, MixerInputProbBundle, MixerInputXBundle, MixerInputBundle}
import paqFe.mixer.Stretch

class ProbGatter(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(p.nFeatures, Vec(8, DecoupledIO(new BitProbBundle()))))
    val inCtx = Flipped(Vec(p.nHidden, Vec(8, DecoupledIO(UInt(8.W)))))

    val out = Vec(8, DecoupledIO(new MixerInputProbBundle()))
    val outCtx = Vec(8, DecoupledIO(Vec(p.nHidden, UInt(8.W))))
  })

  def mixerInputGatter(in: Vec[BitProbBundle]) : MixerInputProbBundle = {
    val gattered = Wire(new MixerInputProbBundle)
    gattered.bit := in.head.bit
    gattered.last := in.head.last
    gattered.probs := VecInit(in.map(_.prob))
    gattered
  }

  for(mixerIdx <- 0 until 8) {
    io.out(mixerIdx) <> DecoupledRegSlice(DecoupledSimpleGatter(VecInit(io.in.map(_(mixerIdx))), mixerInputGatter))
    io.outCtx(mixerIdx) <> DecoupledRegSlice(DecoupledSimpleGatter(VecInit(io.inCtx.map(_(mixerIdx)))))
  }
}

class ScatterX(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(8, DecoupledIO(new MixerInputProbBundle())))
    val inCtx = Flipped(Vec(8, DecoupledIO(Vec(p.nHidden, UInt(8.W)))))

    val out = Vec(8, DecoupledIO(new MixerInputXBundle()))
    val outCtx = Vec(8, DecoupledIO(Vec(p.nHidden, UInt(8.W))))
  })
  
  def strench(x: MixerInputProbBundle) : MixerInputXBundle = {
    val xOut = Wire(new MixerInputXBundle())
    xOut.bit := x.bit
    xOut.last := x.last
    xOut.x := VecInit(x.probs.map(Stretch(_)))
    xOut
  }

  io.in zip io.out foreach {case (in, out) =>
    out <> DecoupledMap(in, strench)
  }
  io.inCtx <> io.outCtx
}

class ProbGatterScatter(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(p.nFeatures, Vec(8, DecoupledIO(new BitProbBundle()))))
    val inCtx = Flipped(Vec(p.nHidden, Vec(8, DecoupledIO(UInt(8.W)))))

    val out = Vec(8, DecoupledIO(new MixerInputBundle()))
  })

  val gatter = Module(new ProbGatter())
  val scatter = Module(new ScatterX())

  io.in <> gatter.io.in
  io.inCtx <> gatter.io.inCtx

  gatter.io.out <> scatter.io.in
  gatter.io.outCtx <> scatter.io.inCtx

  for(mixerIdx <- 0 until 8) {
    io.out(mixerIdx).bits.x := scatter.io.out(mixerIdx).bits.x
    io.out(mixerIdx).bits.ctx := scatter.io.outCtx(mixerIdx).bits
    io.out(mixerIdx).bits.bit := scatter.io.out(mixerIdx).bits.bit
    io.out(mixerIdx).bits.last := scatter.io.out(mixerIdx).bits.last

    io.out(mixerIdx).valid := scatter.io.out(mixerIdx).valid && scatter.io.outCtx(mixerIdx).valid
    scatter.io.out(mixerIdx).ready := io.out(mixerIdx).fire
    scatter.io.outCtx(mixerIdx).ready := io.out(mixerIdx).fire
  }
}
