package RITTER2.IFU.ICache

import chisel3._
import chisel3.util._

import RITTER2._
import RITTER2.RTIB._

/**
 * tips:
 * ICache使用流水线设计
 * 当只发生了Cache miss时，需要暂停流水线
 */

/**
 * ICache have not to proccess the align problem
 */
class ICache extends Module with ICacheConfig with L2Config {
    /** core req bus */
    val fetchBus = IO(Flipped(new RTIB1_interface(icacheAddrWid, icacheDataWid)))
    /** external stroage bus */
    val extBus = IO(new RTIB1_interface(icacheAddrWid, icacheDataWid))

    val extReq = RegInit(false.B)
    val extAddr = Reg(UInt(icacheAddrWid.W))
    extBus.req := extReq
    extBus.addr := extAddr
    extBus.wrcs := false.B
    extBus.wmask := 0.U
    extBus.wdata := 0.U

    /** ICache ways array */
    val ways = Array.fill(wayNums)(Module(new ICacheWay()))
    /** lru registers */
    val lrus = Module(new ICache_LRU(wayNums,lineNums))

    // the second step output
    val ways_tags = Wire(Vec(wayNums, UInt(tagWid.W)))
    val ways_datas = Wire(Vec(wayNums, UInt(icacheLineWid.W)))
    val ways_vlds = Wire(Vec(wayNums, Bool()))
    /** cache pipeline ctrl signal */
    val sCtrlVld = Wire(Bool())
    fetchBus.gnt := sCtrlVld
    //
    //receive the req
    //
    val req_vld = fetchBus.req
    val addr = fetchBus.addr
    /** always is zero */
    val offset = addr(offsetWid - 1, 0)
    val index = addr(indexWid + offsetWid - 1, offsetWid)
    val tag = addr(tagWid + indexWid + offsetWid - 1, indexWid + offsetWid)

    for (i <- 0 until wayNums) {
        ways(i).fetch.ren := fetchBus.req && (fetchBus.wrcs === false.B)
        ways(i).fetch.index := index
        ways_vlds(i) := ways(i).fetch.vld
        ways_tags(i) := ways(i).fetch.tag
        ways_datas(i) := ways(i).fetch.data
    }
    lrus.rc.ren := sCtrlVld & fetchBus.req && (fetchBus.wrcs === false.B)
    lrus.rc.index := index
    val lru_sel = lrus.rc.lru_sel
    //
    //stage1
    //
    val req_step1 = Reg(Bool())
    val tag_step1 = Reg(UInt(tagWid.W))
    val index_step1 = Reg(UInt(indexWid.W))
    when(sCtrlVld) {
        req_step1 := req_vld && (fetchBus.wrcs === false.B)
        tag_step1 := tag
        index_step1 := index
    }
    /** where's way is hit and valid */
    val tags_hit = Wire(Vec(wayNums, Bool()))
    for (i <- 0 until wayNums) {
        tags_hit(i) := ways_vlds(i) && (tag_step1 === ways_tags(i))
    }
    val cache_hit = tags_hit.reduce(_ || _)
    val cache_hit_sel = tags_hit.indexWhere(_ === true.B)
    /** if cache miss,set status to false */
    val hit_status = RegInit(true.B)
    /** pipeline ctrl */
    sCtrlVld :=
        Mux(hit_status,
            (Mux(req_step1, cache_hit, true.B) && hit_status),
            extBus.rsp)

    lrus.hit.vld := cache_hit
    lrus.hit.hit_sel := cache_hit_sel
    //
    //stage2
    //
    val data_output = Reg(UInt(icacheLineWid.W))
    val data_vld = RegInit(false.B)
    fetchBus.rsp := data_vld
    fetchBus.rdata := data_output

    /** ICache line refill req for external storage */
    /** when cache miss,have to reload data from ext storage */
    val sIDLE :: sMISS :: Nil = Enum(2)
    val status = RegInit(sIDLE)
    /** the cache way replacement strategy */
    val replace_sel = Reg(UInt(wayNums.W))
    switch(status) {
        is(sIDLE) {
            when(sCtrlVld === false.B) { //cache miss
                hit_status := false.B
                status := sMISS
                extReq := true.B
                extAddr := Cat(tag_step1, index_step1, 0.U(offsetWid.W))
                data_vld := false.B
                replace_sel := lru_sel.asUInt
            }.elsewhen(req_step1){ //cache hit
                data_vld := true.B
                data_output := ways_datas(cache_hit_sel)
            }.otherwise{
                data_vld := false.B
            }
        }
        is(sMISS) {
            when(extBus.gnt) { //have grant for cache
                extReq := false.B
            }
            //wait for rsp
            when(extBus.rsp) {
                hit_status := true.B
                status := sIDLE
                data_vld := true.B
                data_output := extBus.rdata
            }
        }
    }

    /** select a null line to place */
    for(i <- 0 until wayNums){
        ways(i).refill.wen := extBus.rsp && replace_sel(i)
        ways(i).refill.index := index_step1
        ways(i).refill.tag := tag_step1
        ways(i).refill.data := extBus.rdata
    }
    /** ICache have not to proccess the align problem */
}


import RITTER2.IP._

class ICacheTest_top extends Module{
    val debug = IO(Flipped(new RTIB1_interface(40,128)))

    val icache = Module(new ICache)
    val extRAM = Module(new extRAM(40,128))

    icache.extBus <> extRAM.bus
    icache.fetchBus <> debug
}

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class icache_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new ICacheTest_top).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            var hit_cnt = 0
            for(i <- 0 until 1000) {
                //random read
                val addr = Random.nextInt(100)
                dut.debug.addr.poke((addr << 4).U)
                dut.debug.wrcs.poke(false.B)
                dut.debug.wrcs.poke(0.U)
                dut.debug.wdata.poke(0.U)
                dut.debug.req.poke(true.B)
                dut.clock.step(1)
                dut.debug.req.poke(false.B)
                var cnt=0
                while (!dut.debug.rsp.peekBoolean()) {
                    dut.clock.step(1)
                    cnt+=1
                }
                if(cnt<3){
                    hit_cnt+=1
                }
                if(addr == dut.debug.rdata.peek().litValue -1){
                    println("pass!:" + i)
                }
            }
            println("hit_cnt:" + hit_cnt)
            println("hit_rate:" + (hit_cnt.toDouble/2000.0))
        }
    }
}


object ICache_Main extends App{
    (new chisel3.stage.ChiselStage).emitVerilog(
        new ICache,
        Array[String](
            "--target-dir", "Generated",
            "--emission-options","disableRegisterRandomization",
            "--emission-options","disableMemRandomization"))
}















