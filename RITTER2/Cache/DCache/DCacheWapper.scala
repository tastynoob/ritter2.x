package RITTER2.DCache.DCache

import RITTER2.IP._
import RITTER2._
import chisel3._
import chisel3.util._
import chiseltest._
import IFU.ICache._
import org.scalatest.flatspec._

/**
 * save the tag lines
 * @param tag_size  size of the tag (bits)
 * @param line_nums number of lines in the cache
 */
class DCache_tagArray(tag_size:Int,line_nums:Int) extends ICache_tagArray(tag_size,line_nums);

/**
 * save the data lines
 * there have dirty bit
 * @param data_size size of the data (bits)
 * @param line_nums number of lines in the cache
 */
class DCache_dataArray(data_size: Int, line_nums: Int) extends Module {
    val rc = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(log2Up(line_nums).W))
        val data = Output(UInt(data_size.W))
        val dirty = Output(Bool())
    })
    val wc = IO(new Bundle {
        val wen = Input(Bool())
        val wmask = Input(UInt((data_size/8).W))
        val index = Input(UInt(log2Up(line_nums).W))
        val data = Input(UInt(data_size.W))
        val dirty_update = Input(Bool())
        val dirty = Input(Bool())
    })
    //line_nums must be power of 2 and larger than 2,otherwise throw exception
    assert(line_nums > 2 && isPow2(line_nums))
    /** the data_data arrays registers */
    val mem = Module(new RAM_pdula_withmask(data_size, line_nums))
    /** the dirty array */
    val dirty_array = RegInit(VecInit(Seq.fill(line_nums)(false.B)))
    //todo: the dirty array need to update
    when(wc.dirty_update) {
        dirty_array(wc.index) := wc.dirty
    }

    mem.wc.wen := wc.wen
    mem.wc.wmask := wc.wmask
    mem.wc.addr := wc.index
    mem.wc.din := wc.data

    mem.rc.ren := rc.ren
    mem.rc.addr := rc.index
    rc.data := mem.rc.dout

    val dirty = RegNext(dirty_array(rc.index))
    rc.dirty := dirty
}

/** if cache miss need to read lru_rf to replace the cache way
 *
 * way0|way1|lru_array
 *
 * */
class DCache_LRU(wayNums: Int, lineNums: Int) extends ICache_LRU(wayNums, lineNums);


class DCacheWay extends Module with DCacheConfig {
    val rc = IO(new Bundle {
        val ren = Input(Bool())
        val index = Input(UInt(indexWid.W))
        /** output the cache line vld */
        val vld = Output(Bool())
        /** output the cache line tag */
        val tag = Output(UInt(tagWid.W))
        /** output the cache kine dirty lable */
        val dirty = Output(Bool())
        /** output the cache line data */
        val data = Output(UInt(dcacheLineWid.W))
    })
    /** cache line write and set the line vld to true  */
    /** refill have not to set dirty,but memf write need to */
    val wc = IO(new Bundle {
        val wen = Input(Bool())
        val wmask = Input(UInt((dcacheLineWid/8).W))
        val dirty_update = Input(Bool())
        val dirty = Input(Bool())
        val index = Input(UInt(indexWid.W))
        val tag = Input(UInt(tagWid.W))
        val data = Input(UInt(dcacheLineWid.W))
    })
    /** tag array with valid */
    val tag_array = Module(new DCache_tagArray(tagWid, lineNums))
    tag_array.rc.ren := rc.ren
    tag_array.rc.index := rc.index
    rc.vld := tag_array.rc.vld
    rc.tag := tag_array.rc.tag

    /** data array with dirty */
    val data_array = Module(new DCache_dataArray(dcacheLineWid, lineNums))
    data_array.rc.ren := rc.ren
    data_array.rc.index := rc.index
    rc.data := data_array.rc.data
    rc.dirty := data_array.rc.dirty

    /** refill the cache line */
    tag_array.wc.wen := wc.wen
    tag_array.wc.index := wc.index
    tag_array.wc.vld_update := wc.wen
    tag_array.wc.vld := true.B
    tag_array.wc.tag := wc.tag

    data_array.wc.wen := wc.wen
    data_array.wc.wmask := wc.wmask
    data_array.wc.index := wc.index
    data_array.wc.data := wc.data
    data_array.wc.dirty_update := wc.dirty_update
    data_array.wc.dirty := wc.dirty
}



