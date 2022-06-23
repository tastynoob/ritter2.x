package RITTER2.IFU


import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import RITTER2._
import RITTER2.Utils._

/**
 * generate pc to fetch bus
 */
class IFU_PC extends Module with ICacheConfig {
    /** pc_reg increment value */
    val pc_inc = 1.U << offsetWid.U
    val com = IO(new Bundle {
        val vld = Input(Bool())
        val clear = Input(Bool())
        val clear_pc = Input(UInt(GConfig.pcWid.W))
    })
    val out = IO(new Bundle {
        val vld = Output(Bool())
        val pc = Output(UInt(32.W))
    })
    val vld = RegInit(false.B)
    val pc_reg = RegInit(GConfig.default_pc.U(GConfig.pcWid.W))

    when(com.vld) {
        pc_reg := pc_reg + pc_inc
    }

    vld := com.vld && !com.clear
    /** higher level */
    when(com.clear) {
        pc_reg := com.clear_pc
    }

}


class IFU_S1 extends Module {
    val com = IO(new Bundle {
        val vld = Input(Bool())
    })
    val upper = IO(new Bundle {
        val vld = Input(Bool())
        val pc = Input(UInt(GConfig.pcWid.W))
    })
    val lower = IO(new Bundle {
        val vld = Output(Bool())
        val pc = Output(UInt(GConfig.pcWid.W))
    })

    when(com.vld) {
        lower.vld := upper.vld
        lower.pc := upper.pc
    }
}


/**
 * just do some operation on pc
 * split the full pc to more specifically pc for each inst(32bits)
 */
class IFU_S2 extends Module {
    /** the number of inst(32bits) fetched at one time */
    val InstNums = GConfig.fetchWid / 32
    val com = IO(new Bundle {
        val vld = Input(Bool())
    })
    /** the upper level pipeline input */
    val upper = IO(new Bundle {
        val vld = Input(Bool())
        val pc = Input(UInt(GConfig.pcWid.W))
    })
    /** the lower level pipeline output */
    val lower = IO(new Bundle {
        val vld = Output(Bool())
        val inst_pcs = Output(Vec(InstNums, UInt(GConfig.pcWid.W)))
    })
    val inst_pcs = Wire(Vec(InstNums, UInt(GConfig.pcWid.W)))
    val inst_pcs_reg = Reg(Vec(InstNums, UInt(GConfig.pcWid.W)))

    /** base inst pc */
    for (i <- 0 until InstNums) {
        inst_pcs(i) := upper.pc + (4 * i).U
    }
    when(com.vld) {
        lower.vld := upper.vld
        for (i <- 0 until InstNums) {
            inst_pcs_reg(i) := inst_pcs(i)
        }
    }
}


/**
 * cache line align process
 * [15|14....1|0]
 * */
class IFU_Align extends Module with ICacheConfig {
    val InstNums = GConfig.fetchWid / 32

    def LshifterBy2Byte(x: UInt, shift: UInt): UInt = {
        val shiftWid = log2Up(GConfig.fetchWid / (8 * 2))
        val shifters = Array.fill(shiftWid) {
            Wire(UInt(GConfig.fetchWid.W))
        }
        shifters(0) := Mux(
            shift(0),
            Cat(x(x.getWidth - 16, 0), 0.U(16.W)),
            x
        )
        for (i <- 1 until shiftWid) {
            shifters(i) := Mux(
                shift(i),
                Cat(shifters(i - 1)(shifters(i - 1).getWidth - 16 * (1 << i), 0), 0.U((16 * (1 << i)).W)),
                shifters(i - 1)
            )
        }
        shifters(shiftWid - 1)
    }

    def RshifterBy2Byte(x: UInt, shift: UInt): UInt = {
        val shiftWid = log2Up(GConfig.fetchWid / (8 * 2))
        val shifters = Array.fill(shiftWid) {
            Wire(UInt(GConfig.fetchWid.W))
        }
        shifters(0) := Mux(
            shift(0),
            Cat(0.U(16.W), x(x.getWidth - 1, 16)),
            x
        )
        for (i <- 1 until shiftWid) {
            shifters(i) := Mux(
                shift(i),
                Cat(0.U((16 * (1 << i)).W), shifters(i - 1)(shifters(i - 1).getWidth - 1, 16 * (1 << i))),
                shifters(i - 1)
            )
        }
        shifters(shiftWid - 1)
    }

    val com = IO(new Bundle {
        val vld = Input(Bool())
    })
    val upper = IO(new Bundle {
        val base_pc = Input(UInt(GConfig.pcWid.W))
        val idata_vld = Input(Bool())
        val idata = Input(UInt(GConfig.fetchWid.W))
    })
    val lower = IO(new Bundle {
        val inst_pcs = Output(Vec(InstNums, UInt(GConfig.pcWid.W)))
        val idata_vld = Output(Bool())
        val idata = Output(UInt(GConfig.fetchWid.W))
    })
    val leftflow_buffer = Reg(UInt(GConfig.fetchWid.W))
    val align_offset = upper.base_pc(offsetWid - 1, 0)

    val leftover_buffer_ok = RegInit(false.B)
    val align2_offset = Reg(UInt((GConfig.fetchWid / (8 * 2)).W))
    /** */
    val isAlign = upper.base_pc(offsetWid - 1, 0) === 0.U || (leftover_buffer_ok)

    when(com.vld && upper.idata_vld) {
        when(!isAlign) {
            /** when is not align,save the first line data,then get the second line data and merge */
            leftflow_buffer := RshifterBy2Byte(upper.idata, (align_offset >> 1))
            align2_offset := (GConfig.fetchWid / (8 * 2)).U - align_offset(offsetWid - 1, 1)
            leftover_buffer_ok := true.B
        }.otherwise {
            leftover_buffer_ok := false.B
        }
    }
    val merge = leftflow_buffer | LshifterBy2Byte(upper.idata, align2_offset).asUInt
    /** if the first time is not align,then the next time must also be not align */
    lower.idata_vld := Mux(isAlign, upper.idata_vld, leftover_buffer_ok && upper.idata_vld)
    lower.idata := Mux(isAlign, upper.idata, merge)
}

/**
 * save the inst data to the buffer link to idu
 * and send the inst data to the BPU
 */
class IFU_S3 extends Module{
    val com = IO(new Bundle {
        val vld = Input(Bool())
        /** stop ifu pipeline */
        val stop_genpc = Output(Bool())
    })
    val upper = IO(new Bundle{
        val inst_vld = Input(Bool())
        val inst_pcs = Input(Vec(GConfig.fetchWid / 32, UInt(GConfig.pcWid.W)))
        val idata_vld = Input(Bool())
        val idata = Input(UInt(GConfig.fetchWid.W))
    })
    val toBPU = IO(new Bundle{
        val vld = Output(Bool())
        val inst_pcs = Output(Vec(GConfig.fetchWid / 32, UInt(GConfig.pcWid.W)))
        val idata = Output(UInt(GConfig.fetchWid.W))
    })

    //
    //when pc is ready and inst is not ready,then stop the pipeline
    //
    com.stop_genpc := Mux(upper.inst_vld,upper.idata_vld,false.B)
    when(com.vld){
        when(upper.inst_vld && upper.idata_vld){
            toBPU.vld := true.B
            toBPU.inst_pcs := upper.inst_pcs
            toBPU.idata := upper.idata
        }.elsewhen(!upper.inst_vld && upper.idata_vld){//wait for inst
            toBPU.vld := false.B
        }.elsewhen(upper.inst_vld && !upper.idata_vld){//invalid inst
            toBPU.vld := false.B
        }
    }


}


class IFU_Align_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new IFU_Align).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

        }
    }
}




