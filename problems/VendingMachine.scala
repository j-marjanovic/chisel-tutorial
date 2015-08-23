package TutorialProblems

import Chisel._

class VendingMachine extends Module {
  val io = new Bundle {
    val nickel = Bool(INPUT) // 5 cents
    val dime   = Bool(INPUT) // 10 cents
    val valid  = Bool(OUTPUT) }
  val sIdle :: s5 :: s10 :: s15 :: sOk :: Nil = 
    Enum(UInt(), 5)
  val state = Reg(init=sIdle)

  switch (state) {
    is (sIdle) {
      when (io.nickel) {
        state := s5
      } .elsewhen (io.dime) {
        state := s10
      }
    }
    is (s5) {
      when (io.nickel) {
        state := s10
      } .elsewhen (io.dime) {
        state := s15
      }
    }
    is (s10) {
      when (io.nickel) {
        state := s15
      } .elsewhen (io.dime) {
        state := sOk
      }
    }
    is (s15) {
      when (io.nickel) {
        state := sOk
      } .elsewhen (io.dime) {
        state := sOk // sOk5
      }
    }
    is (sOk) {
      state := sIdle
    }
  }

  io.valid := (state === sOk)
}

class VendingMachineTests(c: VendingMachine) extends Tester(c) {  
  var money = 0
  var isValid = false
  for (t <- 0 until 20) {
    val coin     = rnd.nextInt(3)*5
    val isNickel = coin == 5
    val isDime   = coin == 10

    // Advance circuit
    poke(c.io.nickel, Bool(isNickel).litValue())
    poke(c.io.dime,   Bool(isDime).litValue())
    step(1)

    // Advance model
    money = if (isValid) 0 else (money + coin)
    isValid = money >= 20

    // Compare
    expect(c.io.valid, Bool(isValid).litValue())
  }
}
