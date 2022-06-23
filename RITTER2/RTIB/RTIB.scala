package RITTER2.RTIB

import chisel3._
import chisel3.util._

/**
 * ritter internal bus version 1 for master
 * one addr bus,one read channel and one write channel
 * @param addrBus_wid
 * @param dataBus_wid
 */
class RTIB1_interface(addrBus_wid:Int,dataBus_wid:Int) extends Bundle{
    assert(dataBus_wid>=8 && dataBus_wid%8==0)
//    val clk = Input(Bool())
//    val rst = Input(Bool())
    /** master's addr req */
    val addr = Output(UInt(addrBus_wid.W))
    /** master's */
    val wrcs = Output(Bool())
    /** master's */
    val wmask = Output(UInt((dataBus_wid/8).W))
    /** master's */
    val wdata = Output(UInt(dataBus_wid.W))
    /** slave's */
    val rdata = Input(UInt(dataBus_wid.W))
    /** master request for data  */
    val req = Output(Bool())
    /** slave has grant for master in the same step */
    val gnt = Input(Bool())
    /** slave has response for master in the next step */
    val rsp = Input(Bool())
}

/**
 * ritter internal bus version 2 for slave
 * one read bus and one write bus
 * @param addrBus_wid
 * @param dataBus_wid
 */
class RTIB2_interface(addrBus_wid:Int,dataBus_wid:Int) extends Bundle{
    val clk = Input(Bool())
    val rst = Input(Bool())
}