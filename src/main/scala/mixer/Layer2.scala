package paqFe.mixer

import chisel3._
import chisel3.util._
import paqFe.types.BitProbBundle
import paqFe.util.TreeReduce

class MixerLayer2PE(forceFirstProbEven: Boolean = false)(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Bundle {
      val x = Vec(p.nHidden, SInt(p.XWidth))
      val w = Vec(p.nHidden, SInt(p.WeightWidth))
      val reload = Bool()
      val last = Bool()
      val bit = UInt(1.W)
    }))

    val out = DecoupledIO(new BitProbBundle)
    val dw = Output(Vec(p.nHidden, SInt(p.WeightWidth)))
  })

  val sumLatency = log2Ceil(p.VecDotMACNum) / 2
  val vecDotLatency = 2 + sumLatency + 1
  val probSquashLatency = 1
  val lossCalculationLatency = 1
  val latency = vecDotLatency + sumLatency + probSquashLatency + lossCalculationLatency
  
  val pipeReady = WireInit(false.B)

  def sum(a: SInt, b: SInt) = a + b
  def layerOp(i: Int, x: SInt) = if(i % 2 == 0) RegEnable(x, pipeReady) else x
  
  // data path
  val x = RegEnable(io.in.bits.x, io.in.valid && pipeReady)
  val w = RegEnable(io.in.bits.w, io.in.valid && pipeReady)
  val mul = x zip w map {case (x, w) =>
    RegEnable(x * w, pipeReady)
  }
  val dot = TreeReduce[SInt](mul, sum, layerOp(_,_)) >> 16

  val xQueue = Module(new Queue(Vec(p.nHidden, SInt(p.XWidth)), latency, useSyncReadMem = true, pipe = true))
  xQueue.io.enq.valid := io.in.fire
  xQueue.io.enq.bits := io.in.bits.x

  val dotGt2047 = dot > 2047.S
  val dotLtn2048 = dot < -2048.S
  val probStrech = Wire(SInt(p.XWidth))
  probStrech := RegEnable(Mux(dotGt2047, 2047.S, Mux(dotLtn2048, -2048.S, dot)), pipeReady)
  
  
  val probValid = ShiftRegister(io.in.fire, vecDotLatency + probSquashLatency, false.B, pipeReady)
  val bit = ShiftRegister(io.in.bits.bit, vecDotLatency + probSquashLatency, pipeReady)
  val last = ShiftRegister(io.in.bits.last, vecDotLatency + probSquashLatency, pipeReady)
  val notFirst = RegEnable(~last, false.B, probValid)

  val t = RegEnable(Squash(probStrech), pipeReady)
  val prob = if(forceFirstProbEven) Mux(notFirst, t, 2048.U) else t

  val probExpect = 0.U(p.lossWidth) | Cat(bit, 0.U(12.W))

  val lr = 4.U
  val loss = RegEnable((probExpect - prob).asSInt * lr, pipeReady)

  val lossAccumulateValid = ShiftRegister(probValid, lossCalculationLatency, false.B, pipeReady)
  val lossAccumulateReload = ShiftRegister(io.in.bits.reload, vecDotLatency + probSquashLatency + lossCalculationLatency, false.B, pipeReady)
  val dW = Seq.fill(p.nHidden) { RegInit(0.S(p.WeightWidth)) }
  dW zip xQueue.io.deq.bits foreach {case (dw, x) =>
    when(lossAccumulateValid && pipeReady) {
      when(lossAccumulateReload) {
        dw := (x * loss) >> 16
      }.otherwise {
        dw := ((dw << 16) + (x * loss)) >> 16
      }
    }
  }

  xQueue.io.deq.ready := lossAccumulateValid && pipeReady
  pipeReady := io.out.ready

  io.in.ready := pipeReady

  io.out.valid := probValid
  io.out.bits.bit := bit
  io.out.bits.prob := prob
  io.out.bits.last := last

  io.dw zip dW foreach {case (a, b) => a := b}
}

class WeightAdd(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(p.WeightWidth))
    val b = Input(SInt(p.WeightWidth))
    val c = Output(SInt(p.WeightWidth))
  })
  def getSign(x: SInt) = x(x.getWidth - 1)

  val s = io.a + io.b
  val aSign = getSign(io.a)
  val bSign = getSign(io.b)
  val sSign = getSign(s)
  val overflow = ~aSign && ~bSign && sSign
  val underflow = aSign && bSign && ~sSign

  io.c := Mux(overflow, 65535.S, Mux(underflow, -65536.S, s))
}

object WeightAdd {
  def apply(a: SInt, b: SInt)(implicit p: MixerParameter): SInt = {
    val m = Module(new WeightAdd())
    m.io.a := a
    m.io.b := b
    m.io.c
  }
}

class MixerLayer2(forceFirstProbEven: Boolean = false)(implicit p: MixerParameter) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(new Layer2InputBundle))

    val out = DecoupledIO(new BitProbBundle())
  })

  // ctrl singal
  val inReady = WireInit(false.B)
  val updateStall = WireInit(false.B)
  val peReload = WireInit(false.B)

  val weightUpdate = WireInit(false.B)
  val weightReset = WireInit(false.B)

  // data path
  val Ws = Seq.fill(p.nHidden){ RegInit(p.L2WeightInitVal) }
  val pe = Module(new MixerLayer2PE(forceFirstProbEven))

  pe.io.in.bits.w := Ws
  pe.io.in.bits.x := io.in.bits.x
  pe.io.in.bits.bit := io.in.bits.bit
  pe.io.in.bits.last := io.in.bits.last
  pe.io.in.bits.reload := peReload
  pe.io.in.valid := io.in.valid && ~updateStall
  io.in.ready := pe.io.in.ready && ~updateStall

  Ws zip pe.io.dw foreach {case (a, b) =>
    when(weightReset) {
      a := p.L2WeightInitVal
    }.elsewhen(weightUpdate) {
      a := WeightAdd(a, b)
    }
  }

  // ctrl singal gen
  val sLossAcc :: sWait :: sUpdate :: sRest :: Nil = Enum(4)
  val state = RegInit(sLossAcc)
  val cntInc = WireInit(false.B)
  val cntRst = WireInit(false.B)
  val (cnt, cntWrp) = Counter(0 until p.L2BatchSize, cntInc, cntRst)

  
  peReload := cnt === 0.U

  switch(state) {
    is(sLossAcc) {
      updateStall := false.B
      cntInc := io.in.fire

      when(io.in.fire && io.in.bits.last) {
        state := sRest
      }.elsewhen(cntWrp) {
        state := sWait
      }
    }

    is(sWait) {
      updateStall := true.B
      cntInc := io.out.ready
      
      when(cnt === (pe.latency - 1).U) {
        state := sUpdate
      }
    }

    is(sUpdate) {
      cntRst := true.B
      updateStall := true.B
      weightUpdate := true.B

      state := sLossAcc
    }

    is(sRest) {
      cntRst := true.B
      updateStall := true.B
      weightReset := true.B

      state := sLossAcc
    }
  }

  io.out <> pe.io.out

  val latency = pe.latency
}
