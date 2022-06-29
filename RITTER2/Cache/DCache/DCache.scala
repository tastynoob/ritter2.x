package RITTER2.DCache.DCache

import chisel3._
import chisel3.util._
import RITTER2._
import RITTER2.RTIB._
import Utils.myUtils


/**
 * one DCache bank
 * no support write and read alignement
 * the input data must be aligned to the cache line size
 */
class DCacheBank extends Module with DCacheConfig{
    /** core req bus */
    val memfBus = IO(Flipped(new RTIB1_interface(dcacheAddrWid, dcacheLineWid)))
    /** external stroage bus */
    val extBus = IO(new RTIB1_interface(dcacheAddrWid, dcacheLineWid))

    val extReq = RegInit(false.B)
    val extAddr = Reg(UInt(dcacheAddrWid.W))
    val extwrcs = Reg(Bool())
    val extWData = Reg(UInt(dcacheLineWid.W))
    extBus.req := extReq
    extBus.addr := extAddr
    extBus.wrcs := extwrcs
    extBus.wmask := ~(0.U(dcacheLineWid.W))
    extBus.wdata := extWData

    /** DCache ways array */
    val ways = Array.fill(wayNums)(Module(new DCacheWay()))
    /** lru registers */
    val lrus = Module(new DCache_LRU(wayNums,lineNums))

    // the second step output
    val ways_tags = Wire(Vec(wayNums, UInt(tagWid.W)))
    val ways_datas = Wire(Vec(wayNums, UInt(dcacheLineWid.W)))
    val ways_vlds = Wire(Vec(wayNums, Bool()))
    val ways_dirty = Wire(Vec(wayNums, Bool()))
    /** cache pipeline ctrl signal */
    val sCtrlVld = Wire(Bool())
    memfBus.gnt := sCtrlVld
    //
    //receive the req
    //
    val req_vld = memfBus.req
    val addr = memfBus.addr
    /** always is zero */
    val offset = addr(offsetWid - 1, 0)
    val index = addr(indexWid + offsetWid - 1, offsetWid)
    val tag = addr(tagWid + indexWid + offsetWid - 1, indexWid + offsetWid)

    /** whether it is read or write,need to read vld and dirty */
    for (i <- 0 until wayNums) {
        ways(i).rc.ren := memfBus.req
        ways(i).rc.index := index
        ways_vlds(i) := ways(i).rc.vld
        ways_tags(i) := ways(i).rc.tag
        ways_datas(i) := ways(i).rc.data
        ways_dirty(i) := ways(i).rc.dirty
    }
    lrus.rc.ren := sCtrlVld & memfBus.req
    lrus.rc.index := index
    val lru_bitsel = lrus.rc.lru_sel
    val lru_sel = lru_bitsel.indexWhere(_ === true.B)
    //
    //stage1
    //
    val req_step1 = RegInit(false.B)
    val reqwrcs_step1 = Reg(Bool())
    val reqwmask_step1 = Reg(UInt((dcacheLineWid/8).W))
    val reqwdata_step1 = Reg(UInt(dcacheLineWid.W))
    val tag_step1 = Reg(UInt(tagWid.W))
    val index_step1 = Reg(UInt(indexWid.W))
    when(sCtrlVld) {
        req_step1 := req_vld
        reqwrcs_step1 := memfBus.wrcs
        reqwmask_step1 := memfBus.wmask
        reqwdata_step1 := memfBus.wdata
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
    /**cache is miss and the replacement line is dirty*/
    val cache_miss_and_dirty = (!cache_hit) && (lru_bitsel.asUInt & ways_dirty.asUInt).orR


    /** if cache miss,set status to false */
    val hit_status = RegInit(true.B)
    /** pipeline ctrl,when refill or replace need to stop the pipeline */
    sCtrlVld :=
        Mux(hit_status,
            (Mux(req_step1, cache_hit, true.B) && hit_status),
            extBus.rsp)

    lrus.hit.vld := cache_hit
    lrus.hit.hit_sel := cache_hit_sel

    //
    //stage2
    //
    val data_output = Reg(UInt(dcacheLineWid.W))
    val rsp_vld = RegInit(false.B)
    memfBus.rsp := rsp_vld
    memfBus.rdata := data_output

    /** ICache line refill req for external storage */
    /** when cache miss,have to reload data from ext storage */
    val sIDLE :: sREFILL :: sREPLACE ::Nil = Enum(3)
    val status = RegInit(sIDLE)
    val replace_finish = Wire(Bool())
    replace_finish := false.B
    /** the cache way replacement strategy */
    val replace_bitsel = Reg(UInt(wayNums.W))
    val replace_data = Wire(UInt(dcacheLineWid.W))
    /** default is extBus.data */
    replace_data := extBus.rdata
    /*
    1:read and cache hit
    2:write and cache hit
    3:read and cache miss and the replacement line is not dirty
    4:write and cache miss and the replacement line is not dirty
    5:read and cache miss and the replacement line is dirty
    6:write and cache miss and the replacement line is dirty
    ---
    1,2:just read or write cache line
    3,4,5,6: replace the cache line
    4,6: write to the new cache line
    5,6:write the dirty cache line to external storage
     */
    /** memf write and cache hit */
    val memf_wen = Wire(Bool())
    memf_wen := false.B
    val sONCE::sONCE2TWICE::sTWICE::Nil = Enum(3)
    val transmission_status = RegInit(sONCE)
    switch(status) {
        is(sIDLE) {
            when(sCtrlVld === false.B) { //cache miss
                hit_status := false.B
                status := Mux(cache_miss_and_dirty,sREPLACE,sREFILL)

                extReq := true.B
                extAddr := Mux(cache_miss_and_dirty,
                                Cat(ways_tags(lru_sel),index_step1,0.U(offsetWid.W)),//the replacement addr
                                Cat(tag_step1, index_step1, 0.U(offsetWid.W)))//the memf addr
                extwrcs := Mux(cache_miss_and_dirty,true.B,false.B)//write to ext storage
                extWData := ways_datas(lru_sel)

                replace_bitsel := lru_bitsel.asUInt
                rsp_vld := false.B
            }.elsewhen(req_step1){ //cache hit
                rsp_vld := true.B
                when(reqwrcs_step1){//write cache line
                    memf_wen := true.B
                }.otherwise {
                    data_output := ways_datas(cache_hit_sel)
                }
            }.otherwise{// none req
                rsp_vld := false.B
            }
        }
        is(sREFILL) {//when cache miss,need to reload data from ext storage ,when the cache line is not dirty,just to replace
            when(extBus.gnt) { //have grant for cache
                extReq := false.B
            }
            //wait for read ext storage finish
            when(extBus.rsp) {
                when(reqwrcs_step1){//rewrite
                    replace_data := myUtils.merge(extBus.rdata,reqwdata_step1,reqwmask_step1)
                }
                hit_status := true.B
                status := sIDLE
                rsp_vld := true.B
                data_output := extBus.rdata
            }
        }
        is(sREPLACE){//when cache miss,and the cache line is dirty ,need to write the cache line to ext,and read the data to cache
            when(transmission_status=== sONCE2TWICE){
                when(extBus.req){
                    transmission_status := sTWICE
                }
                extReq := false.B // write and read finish
            }
            when(extBus.gnt && (transmission_status===sONCE)) { //wait for write ext storage finish
                transmission_status := sONCE2TWICE
                //req for read ext storage
                //extReq do not change
                extAddr := Cat(tag_step1, index_step1, 0.U(offsetWid.W))//the memf addr
                extwrcs := false.B//read from ext storage
            }
            when(extBus.rsp && (transmission_status=== sTWICE)){
                transmission_status := sONCE
                when(reqwrcs_step1){//rewrite
                    replace_data := myUtils.merge(extBus.rdata,reqwdata_step1,reqwmask_step1)
                }
                hit_status := true.B
                status := sIDLE
                rsp_vld := true.B
                data_output := extBus.rdata
                replace_finish := true.B
            }
        }
    }
    /** write line */
    for(i <- 0 until wayNums){
        ways(i).wc.wen := Mux(memf_wen,tags_hit(i),extBus.rsp && replace_bitsel(i))
        ways(i).wc.wmask := Mux(memf_wen,reqwmask_step1,~(0.U((dcacheLineWid/8).W)))
        ways(i).wc.index := index_step1
        ways(i).wc.tag := tag_step1
        ways(i).wc.data := Mux(memf_wen,reqwdata_step1,replace_data.asUInt)
        //when memf write to the cache line,whether the cache miss or hit
        //when cache replace need to set the dirty false
        ways(i).wc.dirty_update := (cache_hit && tags_hit(i) && reqwrcs_step1) || ((status =/= sIDLE) && replace_bitsel(i))
        ways(i).wc.dirty := Mux(reqwrcs_step1,
                                true.B,
                                Mux(status=/=sIDLE,false.B,ways_dirty(i)))
    }
    /** ICache have not to proccess the align problem */
}

/**
 * multi bank cache
 *
 */
class DCache extends Module with DCacheConfig {
    /** the memf bus */
    val memfBus = IO(Flipped(new RTIB1_interface(dcacheAddrWid,dcacheDataWid)))
    /** the external storage bus */
    val extBus = IO(new RTIB1_interface(dcacheAddrWid,dcacheLineWid))
}



import RITTER2.IP._

class DCacheBankTest_top extends Module{
    val debug = IO(Flipped(new RTIB1_interface(20,128)))
    val dcache = Module(new DCacheBank)
    val extRAM = Module(new extRAM_without_init(20,128))
    dcache.extBus <> extRAM.bus
    dcache.memfBus <> debug
}

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class dcache_Test extends AnyFlatSpec with ChiselScalatestTester {
    val annos = Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)
    behavior of "Module"
    it should "work" in {
        test(new DCacheBankTest_top).withAnnotations(annos) { dut =>
            var hit_cnt = 0
            for (i <- 0 until 10) {
                println(i)
                //random read
                val addr = Random.nextInt(100)
                dut.debug.addr.poke((addr << 4).U)
                dut.debug.wrcs.poke(false.B)
                dut.debug.wdata.poke(0.U)
                dut.debug.req.poke(true.B)
                dut.clock.step(1)
                dut.debug.req.poke(false.B)
                var cnt = 0
                while (!dut.debug.rsp.peekBoolean()) {
                    dut.clock.step(1)
                    cnt += 1
                }
                if (cnt < 3) {
                    hit_cnt += 1
                }
                if (addr == dut.debug.rdata.peek().litValue - 1) {
                    println("pass!:" + i)
                }else{
                    println("fail!:" + i)
                }
            }
            println("hit_cnt:" + hit_cnt)
            println("hit_rate:" + (hit_cnt.toDouble / 1000.0))
        }
    }
}

object DCache_Main extends App{
    (new chisel3.stage.ChiselStage).emitVerilog(
        new DCacheBankTest_top,
        Array[String](
            "--target-dir", "Generated",
            "--emission-options","disableRegisterRandomization",
            "--emission-options","disableMemRandomization"))
}


