package TutorialProblems

import Chisel._

class Mul extends Module {
  val io = new Bundle {
    val x   = UInt(INPUT,  4)
    val y   = UInt(INPUT,  4)
    val z   = UInt(OUTPUT, 8)
  }
  val muls = Vec.fill( 4 ) {UInt(width = 8)}

  for ( i <- 0 to 3) {
    when (io.y(i) === UInt(1)) {
      muls(i) := io.x << UInt(i)
    } .otherwise {
      muls(i) := UInt(0)
    }
  }

  io.z := muls.reduceLeft(( a : UInt, b : UInt) => a + b)
}

class MulTests(c: Mul) extends Tester(c) {
  val maxInt  = 1 << 4
  for (i <- 0 until 10) {
    val x = rnd.nextInt(maxInt)
    val y = rnd.nextInt(maxInt)
    poke(c.io.x, x)
    poke(c.io.y, y)
    step(1)
    expect(c.io.z, (x * y))
  }
}
