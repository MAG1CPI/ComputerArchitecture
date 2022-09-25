import chisel3._
import chisel3.util._
class md5round extends Module {
  // define: io
  val io = IO(new Bundle {
    // input: A, B, C, D
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val c = Input(UInt(32.W))
    val d = Input(UInt(32.W))
    // input: message section, S[i], K[i], round
    val m = Input(UInt(32.W))
    val s = Input(UInt(5.W))
    val t = Input(UInt(32.W))
    val r = Input(UInt(2.W))
    // output: result
    val next_a = Output(UInt(32.W))
  })
  // define: intermediate result
  val res = Wire(UInt(32.W))

  // define: nonlinear function F, G, H, I
  def F(x: UInt, y: UInt, z: UInt): UInt = {
    (x & y) | ((~x).asUInt & z)
  }
  def G(x: UInt, y: UInt, z: UInt): UInt = {
    (x & z) | (y & (~z).asUInt)
  }
  def H(x: UInt, y: UInt, z: UInt): UInt = {
    x ^ y ^ z
  }
  def I(x: UInt, y: UInt, z: UInt): UInt = {
    y ^ (x | (~z).asUInt)
  }

  // init: wires
  res := 0.U

  // compute one round
  // nonlinear transform according to round(io.r)
  switch(io.r) {
    is(0.U) {
      res := io.a + F(io.b, io.c, io.d) + io.m + io.t
    }
    is(1.U) {
      res := io.a + G(io.b, io.c, io.d) + io.m + io.t
    }
    is(2.U) {
      res := io.a + H(io.b, io.c, io.d) + io.m + io.t
    }
    is(3.U) {
      res := io.a + I(io.b, io.c, io.d) + io.m + io.t
    }
  }
  // get result by rotating Left and adding io.b
  io.next_a := io.b + ((res << io.s) | (res >> (32.U - io.s)))
}
