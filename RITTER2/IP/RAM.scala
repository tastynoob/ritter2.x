package RITTER2.IP

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


/**
 * pseudo dual port ram without mask
 * @param wid  width of the RAM element（bits）can be any number
 * @param dept depth of the RAM
 */
class RAM_pdual(wid: Int, dept: Int) extends Module {
    val rc = IO(new Bundle{
        val ren = Input(Bool())
        val addr = Input(UInt(log2Up(dept).W))
        val dout = Output(UInt(wid.W))
    })
    val wc = IO(new Bundle{
        val wen = Input(Bool())
        val addr = Input(UInt(log2Up(dept).W))
        val din = Input(UInt(wid.W))
    })
    val mem = Mem(dept, UInt(wid.W))
    val dout = Reg(UInt(wid.W))

    when(wc.wen) {
        mem(wc.addr) := wc.din
    }
    when(rc.ren) {
        dout := mem(rc.addr)
    }
    rc.dout := dout
}


/**
 * RAM with write mask
 * write channel and read channel use the same address
 *
 * @param wid  width of the RAM element（bits）must be multiple of 8
 * @param dept depth of the RAM
 */
class RAM(wid: Int, dept: Int) extends Module {
    val io = IO(new Bundle {
        val wen = Input(Bool())
        val wmask = Input(UInt((wid / 8).W))
        val ren = Input(Bool())
        val addr = Input(UInt(log2Up(dept).W))
        val din = Input(UInt(wid.W))
        val dout = Output(UInt(wid.W))
    })
    //wid必须是8的倍数
    assert(wid % 8 == 0 && wid > 0)
    //dept必须是2的幂
    assert(isPow2(dept) && dept > 0)

    val Byte_nums = wid / 8
    //可读可写的RAM
    val mem = Mem(dept, Vec(Byte_nums, UInt(8.W)))
    val dout = Reg(UInt(wid.W))

    when(io.wen) {
        for (i <- 0 until Byte_nums) {
            when(io.wmask(i)) {
                mem(io.addr)(i) := io.din(i * 8 + 7, i * 8)
            }
        }
    }
    when(io.ren) {
        dout := mem(io.addr).asUInt
    }
    io.dout := dout
}

/**
 * the true dual port ram
 * @param wid
 * @param dept
 */
class RAM_dual(wid:Int,dept:Int)extends Module{
    val io = IO(new Bundle{

    })

}


class Ram_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new RAM(32, 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.io.ren.poke(false.B)
            dut.io.wen.poke(true.B)
            dut.io.wmask.poke("b1101".U)
            dut.io.addr.poke(24.U)
            dut.io.din.poke(0x1aaaaaaa.U)
            dut.clock.step(1)

            dut.io.ren.poke(true.B)
            dut.io.wen.poke(false.B)
            dut.clock.step(1)
            //十六进制输出
            println(dut.io.dout.peek.litValue.toInt.toHexString)

        }
    }
}
