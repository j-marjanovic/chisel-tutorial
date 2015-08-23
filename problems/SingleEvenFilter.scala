package TutorialProblems

import Chisel._

abstract class Filter[T <: Data](dtype: T) extends Module {
  val io = new Bundle {
    val in  = Valid(dtype).asInput
    val out = Valid(dtype).asOutput
} }

class PredicateFilter[T <: Data](dtype: T, f: T => Bool) extends Filter(dtype) {
  io.out.valid := io.in.valid && f(io.in.bits)
  io.out.bits  := io.in.bits
}

object SingleFilter {
  def apply[T <: UInt](dtype: T) = 
    Module(new PredicateFilter(dtype, (x: T) => x < UInt(10) ))
}

object EvenFilter {
  def apply[T <: UInt](dtype: T) = 
    Module(new PredicateFilter(dtype, (x: T) => x(0) === UInt(0) ))
}

class SingleEvenFilter[T <: UInt](dtype: T) extends Filter(dtype) {
  val singleFilt = SingleFilter(dtype)
  val evenFilt = EvenFilter(dtype)

  io.in <> singleFilt.io.in
  singleFilt.io.out <> evenFilt.io.in
  io.out <> evenFilt.io.out
}

class SingleEvenFilterTests[T <: UInt](c: SingleEvenFilter[T]) extends Tester(c) {
  val maxInt  = 1 << 16
  for (i <- 0 until 10) {
    val in = rnd.nextInt(maxInt)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits, in)
    val isSingleEven = (in <= 9) && (in%2 == 1)
    step(1)
    expect(c.io.out.valid, Bool(isSingleEven).litValue())
    expect(c.io.out.bits, in)
  }
}

