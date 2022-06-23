package RITTER2.Utils

import chisel3._
import chisel3.stage.ChiselStage
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 *
 * @param eleT fifo硬件元素类型
 * @param dept fifo硬件最大容量
 */
class fifo[T <: Data](eleT: T, dept: Int) extends Module {
    val io = IO(new Bundle {
        val din = Input(eleT)
        val wen = Input(Bool())
        val wer = Output(Bool())
        val dout = Output(eleT)
        val ren = Input(Bool())
        val rer = Output(Bool())
    })
    val fifo_datas = Reg(Vec(dept, eleT))
    val wptr = RegInit(0.U(log2Up(dept + 1).W))


    for (i <- 0 until dept - 1) {
        when(io.ren && io.rer) {
            fifo_datas(i) := fifo_datas(i + 1)
        }
    }
    when(io.wen && io.ren) { //w and r
        wptr := wptr
        fifo_datas(wptr - 1.U) := io.din
    }.elsewhen(io.wen && io.wer) { // just w
        wptr := wptr + 1.U
        fifo_datas(wptr) := io.din
    }.elsewhen(io.ren && io.rer) { // just r
        wptr := wptr - 1.U
    }.otherwise { // others

    }
    io.wer := wptr < dept.U //允许写入
    io.rer := wptr > 0.U //允许读
    io.dout := fifo_datas(0)
}

/**
 * fifo multi-in multi-out
 *
 * @param eleT   fifo element type
 * @param dept   fifo depth
 * @param wports fifo write ports
 * @param rports fifo read ports
 */
class fifox[T <: Data](eleT: T, dept: Int, wports: Int, rports: Int) extends Module {
    def getNums(x:UInt):UInt = {
        val nums = Wire(UInt(log2Up(x.getWidth + 1).W))
        nums := 0.U
        for(i <- 0 until x.getWidth){
            when(x(i)){
                nums := (i+1).U
            }
        }
        nums
    }
    val wc = IO(new Bundle {
        val wen = Input(UInt(wports.W))
        val din = Input(Vec(wports, eleT))
        /** write ready */
        val wer = Output(UInt(wports.W))
    })
    val rc = IO(new Bundle {
        val ren = Input(UInt(rports.W))
        val dout = Output(Vec(rports, eleT))
        /** read ready */
        val rer = Output(UInt(rports.W))
    })
    val wer = Wire(Vec(wports,Bool()))
    val rer = Wire(Vec(rports,Bool()))
    wc.wer := wer.asUInt
    rc.rer := rer.asUInt

    val que = Mem(dept, eleT)
    val wptr = Reg(Vec(wports,UInt(log2Up(dept + 1).W)))
    val rptr = Reg(Vec(rports,UInt(log2Up(dept + 1).W)))
    val cnt = RegInit(0.U(log2Up(dept + 1).W))

    val writeNums = getNums(wc.wen)
    val readNums = getNums(rc.ren)
    for (i <- 0 until wports) {
        when(wc.wen(0)) {
            when(wptr(i) + writeNums >= dept.U(32.W)){
                wptr(i) := wptr(i) + writeNums - dept.U
            }.otherwise{
                wptr(i) := wptr(i) + writeNums
            }
        }
        when(wc.wen(i)){
            que(wptr(i)) := wc.din(i)
        }
        when(cnt < (dept-i).U(32.W)){
            wer(i) := true.B
        }.otherwise{
            wer(i) := false.B
        }
    }
    for(i <- 0 until rports){
        when(rc.ren(0)){
            when(rptr(i) + readNums >= dept.U(32.W)){
                rptr(i) := rptr(i) + readNums - dept.U
            }.otherwise{
                rptr(i) := rptr(i) + readNums
            }
        }
        rc.dout(i) := que(rptr(i))
        when(cnt > i.U){
            rer(i):=true.B
        }.otherwise{
            rer(i):=false.B
        }
    }
    when(wc.wen(0) || rc.ren(0)){
        cnt := cnt + writeNums - readNums
    }
    for(i <- 0 until wports){
        when(reset.asBool){
            wptr(i) := i.U
        }

    }
    for(i <- 0 until rports){
        when(reset.asBool){
            rptr(i) := i.U
        }
    }
}


class fifox_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new fifox(UInt(32.W), 64,4,2)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            dut.wc.wen.poke("b11".U)
            dut.clock.step(10)
            dut.rc.ren.poke("b1".U)
            dut.clock.step(100)
        }
    }
}

object fifox_main extends App{
    (new ChiselStage).emitVerilog(
        new fifox(UInt(16.W), 64,4,4),
        Array[String](
            "--target-dir", "Generated",
            "--emission-options","disableRegisterRandomization",
            "--emission-options","disableMemRandomization"))
}


/**
 * fifo 双端口读写
 * 只允许写使能0,1同时写入
 * 不允许读使能0=false,读使能1=true
 */
class fifo_2sw2r[T <: Data](eleT: T, dept: Int) extends Module {
    val io = IO(new Bundle {
        val din = Input(Vec(2, eleT))
        val wen = Input(Bool())
        val wer = Output(Bool())
        val dout = Output(Vec(2, eleT))
        val ren = Input(Vec(2, Bool()))
        val rer = Output(Vec(2, Bool()))
    })
    //dept必须是偶数,否则报错
    assert(dept >= 2 && dept % 2 == 0) //false抛出异常
    //如果ren(0)=0时,ren(1)=1,则报错
    assert(!(!io.ren(0) && io.ren(1))) //false抛出异常
    /*
    数据排列方式
    3:写1指针,递增
    2:写0指针,递增
    1:读1指针,固定
    0:读0指针,固定
     */

    val fifo_datas = Reg(Vec(dept, eleT))
    val wptr0 = RegInit(0.U(log2Up(dept + 1).W))
    val wptr1 = RegInit(1.U(log2Up(dept + 1).W))


    //注意:一下代码对同一个寄存器进行了赋值
    //越往下的代码生成的verilog判断逻辑优先级更高

    //tag:同时发生2读2写时,直接将写入数据写入wptr-2
    //tag:同时发生2读1写时,直接将写入数据写入wptr-1

    val ren_tag = Wire(Vec(2, Bool()))
    ren_tag(0) := (io.ren(0) && io.rer(0)) //读标志,因为ren(0)为0时,ren(1)必须为0
    ren_tag(1) := (io.ren(1) && io.rer(0)) //压缩个数,0为压缩1,1为压缩2


    //更新数据堆,读取数据时,向下压缩
    when(ren_tag(0)) {
        when(ren_tag(1)) { //压缩2
            for (i <- 0 until dept / 2 - 1) {
                fifo_datas(i * 2) := fifo_datas(i * 2 + 2) // 0 = 2
                fifo_datas(i * 2 + 1) := fifo_datas(i * 2 + 3) // 1 = 3
            }
        }.otherwise { //压缩1
            for (i <- 0 until dept - 1) {
                fifo_datas(i) := fifo_datas(i + 1) // 0 = 1
            }
        }
    }

    //写入同时更新指针,同时发生读写时,写入优先级更高
    when(io.wen && io.wer) { //2w
        when(ren_tag(0) && ren_tag(1)) { //2w2r
            fifo_datas(wptr0 - 2.U) := io.din(0)
            fifo_datas(wptr1 - 2.U) := io.din(1)
        }.elsewhen(ren_tag(0)) { //2w1r
            wptr0 := wptr0 + 1.U
            wptr1 := wptr1 + 1.U
            fifo_datas(wptr0 - 1.U) := io.din(0)
            fifo_datas(wptr1 - 1.U) := io.din(1)
        }.otherwise { //2w0r
            wptr0 := wptr0 + 2.U
            wptr1 := wptr1 + 2.U
            fifo_datas(wptr0) := io.din(0)
            fifo_datas(wptr1) := io.din(1)
        }
    }.otherwise { //0w
        when(ren_tag(0) && ren_tag(1)) { //0w2r
            wptr0 := wptr0 - 2.U
            wptr1 := wptr1 - 2.U
        }.elsewhen(ren_tag(0)) { //0w1r
            wptr0 := wptr0 - 1.U
            wptr1 := wptr1 - 1.U
        }.otherwise { //0w0r
        }
    }


    io.wer := wptr1 < dept.U
    io.rer(0) := wptr0 > 0.U
    io.rer(1) := wptr1 > 1.U
    io.dout(0) := fifo_datas(0)
    io.dout(1) := fifo_datas(1)
}

class fifo_2sw2r_Test extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Module"
    it should "work" in {
        test(new fifo_2sw2r(UInt(32.W), 64)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
            //随机数
            val rnd = scala.util.Random
            //生成105个随机数
            val data = (0 until 128).map(_ => rnd.nextInt(100))
            //同时读写
            dut.io.wen.poke(true.B)
            dut.io.din(0).poke(data(0).U)
            dut.io.din(1).poke(data(1).U)
            dut.io.ren(0).poke(false.B)
            dut.io.ren(1).poke(false.B)
            dut.clock.step(1)

            for (i <- 0 until 50) {
                dut.io.wen.poke(true.B)
                dut.io.din(0).poke(data(i * 2 + 2).U)
                dut.io.din(1).poke(data(i * 2 + 3).U)
                dut.io.ren(0).poke(true.B)
                dut.io.ren(1).poke(true.B)
                dut.io.dout(0).expect(data(i * 2).U)
                dut.io.dout(1).expect(data(i * 2 + 1).U)
                dut.clock.step(1)
                println("time: " + i)
            }

        }
    }
}





