package RITTER2.Float

import chisel3._
import chisel3.util._
import RITTER2.Float.FloatUtils._


import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class SatLeftShift(m:Int,n:Int) extends Module{
	val io = IO(new Bundle{
		val in = Input(UInt(m.W))
		val by = Input(UInt(n.W))
		val out = Output(UInt(m.W))
	})
	io.out := Mux(io.by > m.U, 0.U, io.in >> io.by)
}

class FloatAdd_stage1(n:Int) extends Module{
	val (expWid,manWid) = getExpMantWidths(n)
	val io =IO(new Bundle {
		val a = Input(UInt(n.W))
		val b = Input(UInt(n.W))

		val b_larger = Output(Bool())
		val mant_shift = Output(UInt(expWid.W))
		val exp = Output(UInt(expWid.W))
		val manta = Output(UInt(manWid.W))
		val mantb = Output(UInt(manWid.W))
		val sign = Output(Bool())
		val sub = Output(Bool())
	})
	//定义一个值为0,位宽为32位的常量


	val a_wrap = new FloatWrapper(io.a)
	val b_wrap = new FloatWrapper(io.b)

	val ext_exp_a = Cat(0.U(1.W), a_wrap.exponent)
	val ext_exp_b = Cat(0.U(1.W), b_wrap.exponent)
	val exp_diff = ext_exp_a - ext_exp_b

	val reg_b_larger = Reg(Bool())
	val reg_mant_shift = Reg(UInt(expWid.W))
	val reg_exp = Reg(UInt(expWid.W))
	val reg_manta = RegNext(a_wrap.mantissa)
	val reg_mantb = RegNext(b_wrap.mantissa)
	val reg_sign = Reg(Bool())
	val reg_sub = RegNext(a_wrap.sign ^ b_wrap.sign)

	when (exp_diff(expWid) === 1.U) {
		// absolute value
		reg_mant_shift := -exp_diff(expWid - 1, 0)
		//mant_shift := (~exp_diff) + UInt(1)
		reg_b_larger := true.B
		reg_exp := b_wrap.exponent
		reg_sign := b_wrap.sign
	} .otherwise {
		reg_mant_shift := exp_diff(expWid - 1, 0)
		reg_b_larger := false.B
		reg_exp := a_wrap.exponent
		reg_sign := a_wrap.sign
	}

	io.mant_shift := reg_mant_shift
	io.b_larger := reg_b_larger
	io.exp := reg_exp
	io.manta := reg_manta
	io.mantb := reg_mantb
	io.sign := reg_sign
	io.sub := reg_sub
}


class FloatAdd_stage2(val n: Int) extends Module {
	val (expWidth, mantWidth) = getExpMantWidths(n)

	val io = IO(new Bundle {
		val manta_in = Input(UInt(mantWidth.W+1.W))
		val mantb_in = Input(UInt(mantWidth.W+1.W))
		val exp_in = Input(UInt(expWidth.W))
		val mant_shift = Input(UInt(expWidth.W))
		val b_larger = Input(Bool())
		val sign_in = Input(Bool())
		val sub_in = Input(Bool())

		val manta_out =	Output(UInt(mantWidth.W+1.W))
		val mantb_out = Output(UInt(mantWidth.W+1.W))
		val exp_out = Output(UInt(expWidth.W))
		val sign_out = Output(Bool())
		val sub_out = Output(Bool())
	})

	// in stage 2 we shift the appropriate mantissa by the amount
	// detected in stage 1

	val larger_mant = Wire(UInt(mantWidth.W+1.W))
	val smaller_mant = Wire(UInt(mantWidth.W+1.W))

	when (io.b_larger) {
		larger_mant := io.mantb_in
		smaller_mant := io.manta_in
	} .otherwise {
		larger_mant := io.manta_in
		smaller_mant := io.mantb_in
	}

	val shifted_mant = Mux(io.mant_shift > (mantWidth + 1).U,
		(0).U, smaller_mant >> io.mant_shift)
	val reg_manta = RegNext(larger_mant)
	val reg_mantb = RegNext(shifted_mant)
	val reg_sign = RegNext(io.sign_in)
	val reg_sub = RegNext(io.sub_in)
	val reg_exp = RegNext(io.exp_in)

	io.manta_out := reg_manta
	io.mantb_out := reg_mantb
	io.sign_out := reg_sign
	io.sub_out := reg_sub
	io.exp_out := reg_exp
}


class FloatAdd_stage3(val n: Int) extends Module {
	val (expWidth, mantWidth) = getExpMantWidths(n)

	val io = IO(new Bundle {
		val manta = Input(UInt(mantWidth.W + 1.W))
		val mantb = Input(UInt(mantWidth.W + 1.W))
		val exp_in = Input(UInt(expWidth.W))
		val sign_in = Input(Bool())
		val sub = Input(Bool())

		val mant_out = Output(UInt(mantWidth.W + 1.W))
		val sign_out = Output(Bool())
		val exp_out = Output(UInt(expWidth.W))
	})

	// in stage 3 we subtract or add the mantissas
	// we must also detect overflows and adjust sign/exponent appropriately

	val manta_ext = Cat(0.U(1.W), io.manta)
	val mantb_ext = Cat(0.U(1.W), io.mantb)
	val mant_sum = Mux(io.sub, manta_ext - mantb_ext, manta_ext + mantb_ext)

	// here we drop the overflow bit
	val reg_mant = Reg(UInt(mantWidth.W + 1.W))
	val reg_sign = Reg(Bool())
	val reg_exp = Reg(UInt(expWidth.W))

	// this may happen if the operands were of opposite sign
	// but had the same exponent
	when (mant_sum(mantWidth + 1) === 1.U) {
		when (io.sub) {
			reg_mant := -mant_sum(mantWidth, 0)
			reg_sign := !io.sign_in
			reg_exp := io.exp_in
		} .otherwise {
			// if the sum overflowed, we need to shift back by one
			// and increment the exponent
			reg_mant := mant_sum(mantWidth + 1, 1)
			reg_exp := io.exp_in + 1.U
			reg_sign := io.sign_in
		}
	} .otherwise {
		reg_mant := mant_sum(mantWidth, 0)
		reg_sign := io.sign_in
		reg_exp := io.exp_in
	}

	io.sign_out := reg_sign
	io.exp_out := reg_exp
	io.mant_out := reg_mant
}

class FloatAdd_stage4(val n: Int) extends Module {
	val (expWidth, mantWidth) = getExpMantWidths(n)

	val io = IO(new Bundle {
		val exp_in = Input(UInt(expWidth.W))
		val mant_in = Input(UInt(mantWidth.W + 1.W))

		val exp_out = Output(UInt(expWidth.W))
		val mant_out = Output(UInt(mantWidth.W))
	})

	// finally in stage 4 we normalize mantissa and exponent
	// we need to reverse the sum, since we want the find the most
	// significant 1 instead of the least significant 1
	val norm_shift = PriorityEncoder(Reverse(io.mant_in))

	// if the mantissa sum is zero, result mantissa and exponent should be zero
	when (io.mant_in === 0.U) {
		io.mant_out := 0.U
		io.exp_out := 0.U
	} .otherwise {

		io.mant_out := (io.mant_in << norm_shift)(mantWidth - 1, 0)
		io.exp_out := io.exp_in - norm_shift
	}
}


class FloatAdd(val n: Int) extends Module {
	val io = IO(new Bundle {
		val a = Input(UInt(n.W))
		val b = Input(UInt(n.W))
		val res = Output(UInt(n.W))
	})

	val (expWidth, mantWidth) = getExpMantWidths(n)

	val stage1 = Module(new FloatAdd_stage1(n))

	stage1.io.a := io.a
	stage1.io.b := io.b

	val stage2 = Module(new FloatAdd_stage2(n))

	stage2.io.manta_in := stage1.io.manta
	stage2.io.mantb_in := stage1.io.mantb
	stage2.io.exp_in := stage1.io.exp
	stage2.io.sign_in := stage1.io.sign
	stage2.io.sub_in := stage1.io.sub
	stage2.io.b_larger := stage1.io.b_larger
	stage2.io.mant_shift := stage1.io.mant_shift

	val stage3 = Module(new FloatAdd_stage3(n))

	stage3.io.manta := stage2.io.manta_out
	stage3.io.mantb := stage2.io.mantb_out
	stage3.io.exp_in := stage2.io.exp_out
	stage3.io.sign_in := stage2.io.sign_out
	stage3.io.sub := stage2.io.sub_out

	val stage4 = Module(new FloatAdd_stage4(n))

	stage4.io.exp_in := stage3.io.exp_out
	stage4.io.mant_in := stage3.io.mant_out

	io.res := Cat(stage3.io.sign_out, stage4.io.exp_out, stage4.io.mant_out)
}


class FloatAdd_Test extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Module"
	it should "work" in {
		test(new FloatAdd(32)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			//随机数
			val rnd = scala.util.Random
			//生成128个随机浮点数
			val a = Array.fill(128)(rnd.nextFloat())
			val b = Array.fill(128)(rnd.nextFloat())

			for(i <- 0 until 128){
				dut.io.a.poke(Float_byte2Int(10.36f))//1.25
				dut.io.b.poke(Float_byte2Int(10.23f))
				dut.clock.step(1)
				println(s"a:${a(i)} b:${b(i)} res:${Int_byte2Float(dut.io.res.peek().litValue.toInt)}")
			}


		}
	}
}


