package paqFe.mixer

import chisel3._
import chisel3.util._
import paqFe.types.BitProbBundle
import paqFe.types.TreeReduce

class Layer2PE(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new Bundle {
      val x = Vec(p.nHidden, SInt(p.XWidth))
      val w = Vec(p.nHidden, SInt(p.WeightWidth))
      val reload = Bool()
      val bit = UInt(1.W)
    }))

    val out = ValidIO(new BitProbBundle)
    val dw = Output(Vec(p.nHidden, SInt(p.WeightWidth)))
  })

  val vecDotLatency = 3
  val x = RegEnable(io.in.bits.x, io.in.valid)
  val w = RegEnable(io.in.bits.w, io.in.valid)
  val mul = x zip w map {case (x, w) =>
    RegNext(x * w)
  }
  val dot = RegNext(mul.reduce(_ + _) >> 16)
  //val dot = RegNext(TreeReduce[SInt](mul, (a, b) => a + b))

  val xQueue = Module(new Queue(Vec(p.nHidden, SInt(p.XWidth)), 8))
  xQueue.io.enq.valid := io.in.valid
  xQueue.io.enq.bits := io.in.bits.x
  when(io.in.valid) {
    assert(xQueue.io.enq.ready, "FIFO full!")
  }

  val probSquashLatency = 1
  val dotGt2047 = dot > 2047.S
  val dotLtn2048 = dot < -2048.S
  val probStrech = Wire(SInt(p.XWidth))
  probStrech := Mux(dotGt2047, 2047.S, Mux(dotLtn2048, -2048.S, dot))
  val prob = RegNext(Squash(probStrech))
  val probValid = ShiftRegister(io.in.valid, vecDotLatency + probSquashLatency)

  val lossCalculationLatency = 1
  val bit = ShiftRegister(io.in.bits.bit, vecDotLatency + probSquashLatency)
  val probExpect = 0.U(p.lossWidth) | Cat(bit, 0.U(12.W))

  val lr = 3.U // TODO: Learning rate
  val loss = RegNext((probExpect - prob).asSInt * lr)

  val lossAccumulateValid = ShiftRegister(probValid, lossCalculationLatency)
  val lossAccumulateReload = ShiftRegister(io.in.bits.reload, vecDotLatency + probSquashLatency + lossCalculationLatency)
  val dW = Seq.fill(p.nHidden) { RegInit(0.S(p.WeightWidth)) }
  dW zip xQueue.io.deq.bits foreach {case (dw, x) =>
    when(lossAccumulateValid) {
      when(lossAccumulateReload) {
        dw := (x * loss) >> 16
      }.otherwise {
        dw := ((dw << 16) + (x * loss)) >> 16
      }
    }
  }

  xQueue.io.deq.ready := lossAccumulateValid
  when(lossAccumulateValid) {
    assert(xQueue.io.deq.valid, "FIFO empty!")
  }

  io.out.valid := probValid
  io.out.bits.bit := bit
  io.out.bits.prob := prob
  io.out.bits.last := false.B // DontCare ?

  io.dw zip dW foreach {case (a, b) => a := b}

  val latency = vecDotLatency + probSquashLatency + lossCalculationLatency
}

class MixerLayer2(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Layer2InputBundle))

    val out = ValidIO(new BitProbBundle())
  })

  // ctrl singal
  val inReady = WireInit(false.B)
  val peInValid = WireInit(false.B)
  val peReload = WireInit(false.B)

  val weightUpdate = WireInit(false.B)

  // data path
  val Ws = Seq.fill(p.nHidden){ RegInit(p.L2WeightInitVal.S(p.WeightWidth)) }
  val pe = Module(new Layer2PE())

  pe.io.in.bits.w := Ws
  pe.io.in.bits.x := io.in.bits.x
  pe.io.in.bits.bit := io.in.bits.bit
  pe.io.in.bits.reload := peReload
  pe.io.in.valid := peInValid

  Ws zip pe.io.dw foreach {case (a, b) =>
    when(weightUpdate) {
      a := a + b
    }
  }

  // ctrl singal gen
  val sLossAcc :: sWait :: sUpdate :: Nil = Enum(3)
  val state = RegInit(sLossAcc)
  val cntInc = WireInit(false.B)
  val cntRst = WireInit(false.B)
  val (cnt, cntWrp) = Counter(0 until p.L2BatchSize, cntInc, cntRst)

  
  peReload := cnt === 0.U
  peInValid := io.in.fire

  switch(state) {
    is(sLossAcc) {
      inReady := true.B
      cntInc := io.in.fire

      when(cntWrp) {
        state := sWait
      }
    }

    is(sWait) {
      cntInc := true.B
      
      when(cnt === (pe.latency - 1).U) {
        state := sUpdate
      }
    }

    is(sUpdate) {
      cntRst := true.B
      weightUpdate := true.B

      state := sLossAcc
    }
  }

  io.in.ready := inReady
  io.out := pe.io.out
}