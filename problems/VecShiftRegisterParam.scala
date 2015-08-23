package TutorialProblems

import Chisel._

class VecShiftRegisterParam(val n: Int, val w: Int) extends Module {
  val io = new Bundle {
    val in  = UInt(INPUT,  w)
    val out = UInt(OUTPUT, w)
  }

  var delays = Vec.fill(n) { Reg(UInt())}

  delays(0) := io.in

  for(i <- 1 to n-1) {
    delays(i) := delays(i-1)
  }

  io.out := delays(n-1)
}

class VecShiftRegisterParamTests(c: VecShiftRegisterParam) extends Tester(c) { 
  val reg = Array.fill(c.n){ 0 }
  for (t <- 0 until 16) {
    val in = rnd.nextInt(1 << c.w)
    poke(c.io.in, in)
    step(1)
    for (i <- c.n-1 to 1 by -1)
      reg(i) = reg(i-1)
    reg(0) = in
    expect(c.io.out, reg(c.n-1))
  }
}
