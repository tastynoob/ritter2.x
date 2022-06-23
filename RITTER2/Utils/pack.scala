package RITTER2.Utils

import chisel3._
import chisel3.util._

/**
 * a base vld-rdy handshake for master
 * one output for vld, one input for rdy
 */
class SyncHandshake extends Bundle {
    val vld = Output(Bool())
    val rdy = Input(Bool())
}




/** bin to gray code  */
object GrayCode {
    def apply(x: UInt): UInt = {
        val n = x.getWidth.U
        val mask = (1.U << n) - 1.U
        val gray = x ^ (x >> 1.U)
        val g = (gray & mask)
        g
    }
}

/** gray code to bin */
object DeGrayCode {
    def apply(x: UInt): UInt = {
        val n = x.getWidth.U
        val mask = (1.U << n) - 1.U
        val gray = x ^ (x >> 1.U)
        val g = (gray ^ mask)
        g
    }
}

/** shift by 1 byte */
object Shift1Byte {
    def apply(x: UInt): UInt = {
        val res = Cat(x(x.getWidth-1,8), 0.U(8.W))
        res
    }
}

/** shift by 2 byte  */
object Shift2Byte {
    def apply(x: UInt): UInt = {
        val res = Cat(x(x.getWidth-1,16), 0.U(16.W))
        res
    }
}


