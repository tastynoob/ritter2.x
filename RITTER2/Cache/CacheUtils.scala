package RITTER2.Cache

import RITTER2.IP._
import chisel3._
import chisel3.util._



/**
 * save the data lines
 * @param data_size size of the data (bits)
 * @param line_nums number of lines in the cache
 */
class DCache_dataArray(data_size:Int, line_nums:Int) extends Module {
    val com = IO(new Bundle{
        val index = Input(UInt(log2Up(line_nums).W))
    })
    val rc = IO(new Bundle{
        val ren = Input(Bool())
        val dirty = Output(Bool())
        val data = Output(UInt(data_size.W))
    })
    val wc = IO(new Bundle{
        val wen = Input(Bool())
        val wmask = Input(UInt((data_size/8).W))
        val dirty_update = Input(Bool())
        val dirty = Input(Bool())
        val data = Input(UInt(data_size.W))
    })
    //line_nums must be power of 2 and larger than 2,otherwise throw exception
    assert(line_nums > 2 && isPow2(line_nums))

    /** the data_dirty arrays registers */
    val dirty_arrays = RegInit(0.U(line_nums.W))
    /** the data_data arrays registers */
    val mem = Module(new RAM(data_size,line_nums))

    when(wc.dirty_update){
        dirty_arrays(com.index) := wc.dirty
    }

    mem.io.wen := wc.wen
    mem.io.wmask := wc.wmask
    mem.io.ren := rc.ren
    mem.io.addr := com.index
    mem.io.din := wc.data
    val data = mem.io.dout
    val dirty = dirty_arrays(com.index)

    rc.dirty := dirty
    rc.data := data
}


