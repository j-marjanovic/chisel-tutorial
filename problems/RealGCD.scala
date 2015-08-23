package TutorialProblems

import Chisel._

class RealGCDInput extends Bundle {
  val a = Bits(width = 16)
  val b = Bits(width = 16)
}

class RealGCD extends Module {
  val io  = new Bundle {
    val in  = Decoupled(new RealGCDInput()).flip()
    val out = Valid(Bits(width = 16))
  }

  val (s_idle :: // waits for valid
    s_loop :: // returns if one is zero
    Nil) = Enum (UInt(), 2)

  val state = Reg(init=s_idle)
  val a = Reg(UInt(width=io.in.bits.a.getWidth))
  val b = Reg(UInt(width=io.in.bits.b.getWidth))
  val k = Reg(UInt(width=8), init=UInt(1, width=8))

  // default outputs
  io.in.ready := UInt(0)
  io.out.valid := UInt(0)
  io.out.bits := UInt(0)

  io.in.ready := state === s_idle

  switch (state){
    is (s_idle) {
      io.in.ready := UInt(1)
      when (io.in.valid) {
        a := io.in.bits.a
        b := io.in.bits.b
        k := UInt(1)
        state := s_loop
        printf("got a: %d and b: %d\n", io.in.bits.a, io.in.bits.b)
      }
    }
    is (s_loop) {
      when (a === UInt(0)) {
        io.out.bits := b * k
        io.out.valid := UInt(1)
        state := s_idle;
      } .elsewhen (b === UInt(0)) {
        io.out.bits := a * k
        io.out.valid := UInt(1)
        state := s_idle;
      } .elsewhen ((a(0) === UInt(0)) && (b(0) === UInt(0))) {
        printf("both even (%d, %d)\n", a, b)
        a := a >> UInt(1)
        b := b >> UInt(1)
        k := k << UInt(1)
      } .elsewhen ((a(0) === UInt(0)) && (b(0) === UInt(1))) {
        printf("a even, b odd (%d, %d)\n", a, b)
        a := a >> UInt(1)
      } .elsewhen ((a(0) === UInt(1)) && (b(0) === UInt(0))) {
        printf("a odd, b even (%d, %d)\n", a, b)
        b := b >> UInt(1)
      } .otherwise {
        printf("both odd (%d, %d)\n", a, b)
        when (a > b) {
          a := (a-b) >> UInt(1)
        } . otherwise {
          b := (b-a) >> UInt(1)
        }
      }
    }
  }

}

class RealGCDTests(c: RealGCD) extends Tester(c) {
  val inputs = List( (48, 32), (7, 3), (100, 10) )
  val outputs = List( 16, 1, 10)

  var i = 0
  do {
    var transfer = false
    do {
      poke(c.io.in.bits.a, inputs(i)._1)
      poke(c.io.in.bits.b, inputs(i)._2)
      poke(c.io.in.valid,  1)
      transfer = (peek(c.io.in.ready) == 1)
      step(1)
    } while (t < 100 && !transfer)

    do {
      poke(c.io.in.valid, 0)
      step(1)
    } while (t < 100 && (peek(c.io.out.valid) == 0))
    expect(c.io.out.bits, outputs(i))
    i += 1;
  } while (t < 100 && i < 3)
  if (t >= 100) ok = false
}
