package RITTER2.IFU.ICache

import RITTER2.IP._
import RITTER2._
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec._

/**
 * save the tag lines
 *
 * @param tag_size  size of the tag (bits)
 * @param line_nums number of lines in the cache
 */
class ICache_tagArray(tag_size: Int, line_nums: Int) extends Module {
    /** read channel io */
    val rc = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(log2Up(line_nums).W))
        /** current step output of the cache line valid */
        val vld = Output(Bool())
        /** next step output of cache line tag */
        val tag = Output(UInt(tag_size.W))
    })
    /** write channel io */
    val wc = IO(new Bundle {
        val wen = Input(Bool())
        val index = Input(UInt(log2Up(line_nums).W))
        /** update the cache line valid */
        val vld_update = Input(Bool())
        val vld = Input(Bool())
        val tag = Input(UInt(tag_size.W))
    })

    //line_nums must be power of 2 and larger than 2,otherwise throw exception
    assert(line_nums > 2 && isPow2(line_nums))

    /** the tag_valid arrays registers */
    val vld_arrays = RegInit(VecInit(Seq.fill(line_nums)(false.B)))

    /** the tag_tag arrays registers */
    val sram = Module(new RAM_pdual(tag_size, line_nums))
    when(wc.vld_update) {
        vld_arrays(wc.index) := wc.vld
    }

    sram.wc.wen := wc.wen
    sram.wc.addr := wc.index
    sram.wc.din := wc.tag

    sram.rc.ren := rc.ren
    sram.rc.addr := rc.index

    val tag = sram.rc.dout
    val valid = RegNext(vld_arrays(rc.index))

    rc.vld := valid
    rc.tag := tag
}


/**
 * save the data lines
 * there is not have dirty lable
 *
 * @param data_size size of the data (bits)
 * @param line_nums number of lines in the cache
 */
class ICache_dataArray(data_size: Int, line_nums: Int) extends Module {
    val rc = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(log2Up(line_nums).W))
        val data = Output(UInt(data_size.W))
    })
    val wc = IO(new Bundle {
        val wen = Input(Bool())
        val index = Input(UInt(log2Up(line_nums).W))
        val data = Input(UInt(data_size.W))
    })
    //line_nums must be power of 2 and larger than 2,otherwise throw exception
    assert(line_nums > 2 && isPow2(line_nums))
    /** the data_data arrays registers */
    val mem = Module(new RAM_pdual(data_size, line_nums))

    mem.wc.wen := wc.wen
    mem.wc.addr := wc.index
    mem.wc.din := wc.data

    mem.rc.ren := rc.ren
    mem.rc.addr := rc.index
    rc.data := mem.rc.dout
}

/** if cache miss need to read lru_rf to replace the cache way
 *
 * way0|way1|lru_array
 *
 * */
class ICache_LRU(wayNums: Int, lineNums: Int) extends Module {
    val lruWid = (wayNums - 1)
    /** update the lru data when read and output the replace result */
    val rc = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(log2Up(lineNums).W))
        /**
         * lru_sel means the way need to replace when cache miss at the next step
         */
        val lru_sel = Output(Vec(wayNums,Bool()))
    })
    /** if hit */
    val hit = IO(new Bundle {
        val vld = Input(Bool())
        val hit_sel = Input(UInt(wayNums.W))
    })
    /** lru arrays : lineNums * lruWid */
    val lru_rf = Reg(Vec(lineNums, UInt(lruWid.W)))
    val lru_step = Reg(UInt(lruWid.W))
    val index = Reg(UInt(log2Up(lineNums).W))
    val lru_update_sel = Mux(hit.vld, hit.hit_sel, rc.lru_sel.asUInt)
    val lru_update_sel_new_lru = Wire(Vec(lruWid, Bool()))
    val lru_update_vld = RegInit(false.B)
    when(rc.ren) {
        when((rc.index === index) && lru_update_vld){
            lru_step := lru_update_sel_new_lru.asUInt
        }.otherwise {
            lru_step := lru_rf(rc.index)
            index := rc.index
        }
    }
    //
    //update the lru register
    //
    when(rc.ren) {
        lru_update_vld := true.B
    }.otherwise {
        lru_update_vld := false.B
    }

    /** lru reg to lru sel */
    val lru_sel_id = Wire(Vec(wayNums / 2, Vec(log2Up(wayNums), Bool())))
    var p = wayNums / 2
    var k = 0
    for (j <- 0 until log2Up(wayNums)) { //8
        for (i <- 0 until (wayNums / 2)) { //4
            var t = i / p + k // i/8 + 0
            lru_sel_id(i)(log2Up(wayNums) - j - 1) := lru_step(t).asBool
        }
        k = k * 2 + 1
        p = p / 2
    }
    /**
     * lru_reg : 0|10|0011 - > lru_sel : 0|1|0
     */
    for (i <- 0 until wayNums) {
        rc.lru_sel(i) := (lru_sel_id(i / 2).asUInt === i.U)
    }
    /**
     * if hit,then reset the lru_rf by hit_sel
     * if miss,then update the lru_rf by lru_sel,
     */
    k = wayNums // 8
    for (i <- 0 until log2Up(wayNums)) {
        val t = wayNums / (2 << i) - 1 //8/2 - 1 = 3
        for (j <- 0 until (1 << i)) {
            p = (1 << i) - 1 + j
            val m = t + j * k
            val n = m - (k / 2 - 1)
            lru_update_sel_new_lru(p) := Mux(lru_update_sel(m+k/2, n).orR,lru_update_sel(m, n).orR,lru_step(p))
        }
        k /= 2
    }
    when(lru_update_vld) {
        lru_rf(index) := lru_update_sel_new_lru.asUInt
    }
}


class ICacheWay extends Module with ICacheConfig {
    val fetch = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(indexWid.W))
        /** output the cache line vld */
        val vld = Output(Bool())
        /** output the cache line tag */
        val tag = Output(UInt(tagWid.W))
        /** output the cache line data */
        val data = Output(UInt(icacheLineWid.W))
    })
    /** cache line refill and set the line vld to true */
    val refill = IO(new Bundle {
        val wen = Input(Bool())
        val index = Input(UInt(indexWid.W))
        val tag = Input(UInt(tagWid.W))
        val data = Input(UInt(icacheLineWid.W))
    })

    /** tag array with valid */
    val tag_array = Module(new ICache_tagArray(tagWid, lineNums))
    tag_array.rc.ren := fetch.ren
    tag_array.rc.index := fetch.index
    fetch.vld := tag_array.rc.vld
    fetch.tag := tag_array.rc.tag

    /** data array with dirty */
    val data_array = Module(new ICache_dataArray(icacheLineWid, lineNums))
    data_array.rc.ren := fetch.ren
    data_array.rc.index := fetch.index
    fetch.data := data_array.rc.data

    /** refill the cache line */
    tag_array.wc.wen := refill.wen
    tag_array.wc.index := refill.index
    tag_array.wc.vld_update := refill.wen
    tag_array.wc.vld := true.B
    tag_array.wc.tag := refill.tag

    data_array.wc.wen := refill.wen
    data_array.wc.index := refill.index
    data_array.wc.data := refill.data

}


class LRU_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new ICache_LRU(16,32)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            for(i <- 0 until 10) {
                dut.rc.ren.poke(true.B)
                dut.rc.index.poke(7.U)
                dut.clock.step(1)
                println("************")
                println("sel:" + dut.rc.lru_sel.peek().litValue.toInt.toBinaryString) //000
                dut.hit.vld.poke(false.B)
                dut.hit.hit_sel.poke(2.U) //cache miss
            }
        }
    }
}

object LRU_Main extends App{
    (new chisel3.stage.ChiselStage).emitVerilog(
        new ICache_LRU(16,64),
        Array[String](
            "--target-dir", "Generated",
            "--emission-options","disableRegisterRandomization",
            "--emission-options","disableMemRandomization"))
}

