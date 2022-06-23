package RITTER2.IFU


import chisel3._
import chisel3.util._
import RITTER2._
import RITTER2.IFU.ICache._
import RITTER2.RTIB.RTIB1_interface

/*

PC -> stage1 -> stage2 -> stage3
I                      ^
V                      I
ICache -> xx -> xx    +

 */


/**
 *
 *
 *
 */
class IFU extends Module with ICacheConfig{
    val com = IO(new Bundle{
        val vld = Input(Bool())
    })
    /** external storage bus */
    val extBus = IO(new RTIB1_interface(icacheAddrWid,icacheDataWid))


    val ifu_pc = Module(new IFU_PC)
    val icache = Module(new ICache)
    val ifu_s1 = Module(new IFU_S1)
    val ifu_s2 = Module(new IFU_S2)
    val ifu_s3 = Module(new IFU_S3)
    val ifu_align = Module(new IFU_Align)


    icache.extBus <> extBus






}
