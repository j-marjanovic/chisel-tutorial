package TutorialProblems

import Chisel._
import scala.util._

class Max2 extends Module {
  val io = new Bundle {
    val in0 = UInt(INPUT,  8)
    val in1 = UInt(INPUT,  8)
    val out = UInt(OUTPUT, 8)
  }
  io.out := Mux(io.in0 > io.in1, io.in0, io.in1)
}

class Max2Tests(c: Max2) extends Tester(c) {
  // var rnd = Random.seed(1)
  for (i <- 0 until 10) {
    val rnd1 = rnd.nextInt(255)
    val rnd2 = rnd.nextInt(255  )
    poke(c.io.in0, rnd1)
    poke(c.io.in1, rnd2)
    // FILL THIS IN HERE
    step(1)
    expect(c.io.out, if (rnd1 > rnd2) rnd1 else rnd2)
  }
}
