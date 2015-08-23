package TutorialProblems

import Chisel._

class Accumulator extends Module {
  val io = new Bundle {
    val in  = UInt(width = 1, dir = INPUT)
    val out = UInt(width = 8, dir = OUTPUT)
  }

  var accum = Reg(UInt(width = io.out.getWidth()))
  // accum := accum + io.in

  when (io.in === UInt(1)) {
    accum := accum + UInt(1)
  }

  io.out := accum
}

class AccumulatorTests(c: Accumulator) extends Tester(c) {
  var tot = 0
  for (t <- 0 until 16) {
    val in = rnd.nextInt(2)
    poke(c.io.in, in)
    step(1)
    if (in == 1) tot += 1
    expect(c.io.out, tot)
  }
}
