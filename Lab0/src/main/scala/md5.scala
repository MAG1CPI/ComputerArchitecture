import chisel3._
import chisel3.util._

import scala.language.postfixOps
/*
MD5 algorithm
Step:
1. data padding
2. initialization
3. iteration
 */
class md5 extends Module {
  // define: io
  val io = IO(new Bundle {
    // input: input and its validity
    val in = Input(UInt(128.W))
    val in_valid = Input(Bool())
    // output: result and its validity
    val out = Output(UInt(128.W))
    val out_valid = Output(Bool())
    // output: is ready
    val ready = Output(Bool())
  })

  // define: states enum
  // |              states enum                 |
  // | idle  |      work state   |  transition  |
  // | state | r0 | r1 | r2 | r3 | fin | state  |
  val idle :: r0 :: r1 :: r2 :: r3 :: finished :: turn_arnd :: Nil = Enum(7)
  val work = Wire(UInt(1.W))

  // bound registers and wires
  // A, B, C, D
  val A = RegInit(defs.A)
  val B = RegInit(defs.B)
  val C = RegInit(defs.C)
  val D = RegInit(defs.D)
  // original A, B, C, D
  val AA = RegInit(0.U(32.W))
  val BB = RegInit(0.U(32.W))
  val CC = RegInit(0.U(32.W))
  val DD = RegInit(0.U(32.W))
  // next A, B, C, D
  val next_A = Wire(UInt(32.W))
  val next_B = Wire(UInt(32.W))
  val next_C = Wire(UInt(32.W))
  val next_D = Wire(UInt(32.W))
  // status
  val phase = RegInit(0.U(4.W))
  val state = RegInit(1.U(8.W))
  val next_state = Wire(UInt(8.W))
  // message
  val msg = RegInit(0.U(512.W))
  // can output
  val out_r = RegInit(false.B)
  // MD5 round module
  val round = Module(new md5round)

  // init: wires
  work := 0.U
  next_A := A
  next_B := B
  next_C := C
  next_D := D
  next_state := state
  // init: module
  round.io.a := 0.U
  round.io.b := 0.U
  round.io.c := 0.U
  round.io.d := 0.U
  round.io.m := 0.U
  round.io.s := 0.U
  round.io.t := 0.U
  round.io.r := 0.U

  // status
  work := state(r0) | state(r1) | state(r2) | state(r3)
  out_r := state(finished)
  io.out_valid := out_r
  io.ready := state(idle)
  // get output by splicing A, B, C, D
  io.out := A ## B ## C ## D

  // update state
  state := next_state

  // store original A, B, C, D
  when(next_state(idle)) {
    AA := 0.U
    BB := 0.U
    CC := 0.U
    DD := 0.U
  }.elsewhen(next_state(r0) && state(idle)) {
    AA := A
    BB := B
    CC := C
    DD := D
  }

  // update A, B, C, D
  // after starting work, assign A/B/C/D with next_A/next_B/next_C/next_D
  when(next_state(idle)) {
    A := defs.A
    B := defs.B
    C := defs.C
    D := defs.D
  }.otherwise {
    A := next_A
    B := next_B
    C := next_C
    D := next_D
  }

  // update phase
  // after starting work, add 1 each time with phase
  when(work.asBool) {
    phase := phase + 1.U
  }.otherwise {
    phase := 0.U
  }

  // init message
  when(next_state(idle)) {
    msg := 0.U
  }.elsewhen(next_state(r0) && state(idle)) {
    msg := Cat(io.in, io.in, io.in, io.in)
  }

  // combine logic
  // begin
  // when state is idle and input is valid
  // state switch to work(r0)
  when(state(idle) && io.in_valid) {
    printf(p"coding...\n")
    next_state := UIntToOH(r0)
  }

  // work
  // when state is rx(0, 1, 2, 3)
  // 4 rounds calc
  when(work.asBool) {
    // set a, b, c and d according to phase
    switch(phase(1, 0)) {
      is(0.U) {
        round.io.a := A
        round.io.b := B
        round.io.c := C
        round.io.d := D
        next_A := round.io.next_a
      }
      is(1.U) {
        round.io.a := D
        round.io.b := A
        round.io.c := B
        round.io.d := C
        next_D := round.io.next_a
      }
      is(2.U) {
        round.io.a := C
        round.io.b := D
        round.io.c := A
        round.io.d := B
        next_C := round.io.next_a
      }
      is(3.U) {
        round.io.a := B
        round.io.b := C
        round.io.c := D
        round.io.d := A
        next_B := round.io.next_a
      }
    }
    // set r, m, s and t
    switch(state) {
      // round 0
      is("b10".U) {
        // set r according to round
        round.io.r := 0.U
        // set m, s and t according to round and phase
        for (i <- 0 until 16) // turn UInt into Int
          when(phase === i.U) {
            round.io.m := msg(i * 32 + 31, i * 32)
            round.io.s := defs.S(i)
            round.io.t := defs.K(i)
          }
        // if all done, go to next round
        when(phase === 15.U) {
          next_state := UIntToOH(r1)
        }
      }
      // round 1
      is("b100".U) {
        round.io.r := 1.U
        for (i <- 0 until 16)
          when(phase === i.U) {
            val j = (i >> 1) | ((i << 3) & 0xf)
            round.io.m := msg(j * 32 + 31, j * 32)
            round.io.s := defs.S(16 + i)
            round.io.t := defs.K(16 + i)
          }
        when(phase === 15.U) {
          next_state := UIntToOH(r2)
        }
      }
      // round 2
      is("b1000".U) {
        round.io.r := 2.U
        for (i <- 0 until 16)
          when(phase === i.U) {
            val j = (i >> 2) | ((i << 2) & 0xf)
            round.io.m := msg(j * 32 + 31, j * 32)
            round.io.s := defs.S(32 + i)
            round.io.t := defs.K(32 + i)
          }
        when(phase === 15.U) {
          next_state := UIntToOH(r3)
        }
      }
      // round 3
      is("b10000".U) {
        round.io.r := 3.U
        for (i <- 0 until 16)
          when(phase === i.U) {
            val j = (i >> 3) | ((i << 1) & 0xf)
            round.io.m := msg(j * 32 + 31, j * 32)
            round.io.s := defs.S(48 + i)
            round.io.t := defs.K(48 + i)
          }
        when(phase === 15.U) {
          next_state := UIntToOH(finished)
        }
      }
    }
  }

  // end
  // when state is finished
  // finish work state and give the result
  when(state(finished)) {
    printf(p"finished\n")
    // adding original value
    next_A := AA + A
    next_B := BB + B
    next_C := CC + C
    next_D := DD + D
    next_state := UIntToOH(turn_arnd)
  }

  // shift
  // when state is turn_arnd
  // state switch to idle
  when(state(turn_arnd)) {
    printf(p"turn_arnd\n")
    next_state := UIntToOH(idle)
  }
}
