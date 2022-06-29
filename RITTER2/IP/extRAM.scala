package RITTER2.IP

import chisel3._
import chisel3.util._

import RITTER2.RTIB._

class extRAM(addrWid:Int,dataWid:Int) extends Module{
    val bus = IO(Flipped(new RTIB1_interface(addrWid,dataWid)))

    bus.gnt := bus.req
    val mem = Mem(1<<addrWid,UInt(dataWid.W))

    val addr = bus.addr(addrWid-1,log2Up(dataWid/8))

    //initialize the memory
    for(i <- 0 until (1<<addrWid)){
        mem(i) := (i + 1).U
    }
    val rsp = RegInit(false.B)
    val data = Reg(UInt(dataWid.W))
    bus.rsp := rsp
    bus.rdata := data

    val cnt = RegInit(0.U(32.W))
    val vld = RegInit(false.B)
    when(vld){
        cnt := cnt+1.U
        when(cnt >= 10.U){
            rsp := true.B
            vld := false.B
            cnt := 0.U
        }
    }
    when(rsp){
        rsp:=false.B
    }
    when(bus.req){
        vld := true.B
        when(bus.wrcs) {//write
            mem(addr) := bus.wdata
        }.otherwise {//read
            data := mem(addr)
        }
    }
}


class extRAM_without_init(addrWid:Int,dataWid:Int) extends Module{
    val bus = IO(Flipped(new RTIB1_interface(addrWid,dataWid)))

    bus.gnt := bus.req
    val mem = Mem(1<<addrWid,UInt(dataWid.W))

    val addr = bus.addr(addrWid-1,log2Up(dataWid/8))

    val rsp = RegInit(false.B)
    val data = Reg(UInt(dataWid.W))
    bus.rsp := rsp
    bus.rdata := data
    val cnt = RegInit(0.U(32.W))
    val vld = RegInit(false.B)
    when(vld){
        cnt := cnt+1.U
        when(cnt >= 10.U){
            rsp := true.B
            vld := false.B
            cnt := 0.U
        }
    }
    when(rsp){
        rsp:=false.B
    }
    when(bus.req){
        vld := true.B
        when(bus.wrcs) {//write
            mem(addr) := bus.wdata
        }.otherwise {//read
            data := mem(addr)
        }
    }
}
