package RITTER2.Float

import chisel3._
import chisel3.util._

class FloatWrapper(val num: Bits) {
	val (sign, exponent, mantissa, zero) = num.getWidth match {
		case 32 => (
			num(31),
			num(30, 23),
			// if the exponent is 0
			// this is a denormalized number
			Cat(Mux(num(30, 23) === 0.U, 0.U(1.W), 1.U(1.W)), num(22, 0)),
			num(30, 0) === 0.U)
		case 64 => (
			num(63),
			num(62, 52),
			Cat(Mux(num(62, 52) === 0.U, 0.U(1.W), 1.U(1.W)), num(51, 0)),
			num(62, 0) === 0.U)
	}
}