package TutorialProblems

import Chisel._

class VecShiftRegister extends Module {
  val io = new Bundle {
    val ins   = Vec.fill(4){ UInt(INPUT, 4) }
    val load  = Bool(INPUT)
    val shift = Bool(INPUT)
    val out   = UInt(OUTPUT, 4)
  }

  val regs = Vec.fill(4) { Reg(UInt()) }


  when (io.load){
    for (i <- 0 to 3) {
      regs(i) := io.ins(i)
    }
  } .elsewhen (io.shift) {
    for (i <- 1 to 3) {
      regs(i) := regs(i-1)
    }
    regs (0) := io.ins(0)
  }
  io.out := regs(3)
}

class VecShiftRegisterTests(c: VecShiftRegister) extends Tester(c) { 
  val reg     = Array.fill(4){ 0 }
  val ins     = Array.fill(4){ 0 }
  for (t <- 0 until 16) {
    for (i <- 0 until 4)
      ins(i) = rnd.nextInt(16)
    val shift = rnd.nextInt(2)
    val load  = rnd.nextInt(2)
    for (i <- 0 until 4)
      poke(c.io.ins(i), ins(i))
    poke(c.io.load,  load)
    poke(c.io.shift, shift)
    step(1)
    if (load == 1) {
      for (i <- 0 until 4) 
        reg(i) = ins(i)
    } else if (shift == 1) {
      for (i <- 3 to 1 by -1)
        reg(i) = reg(i-1)
      reg(0) = ins(0)
    }
    expect(c.io.out, reg(3))
  }
}
