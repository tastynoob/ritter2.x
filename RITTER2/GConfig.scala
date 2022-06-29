package RITTER2

import chisel3.util._


object GConfig {
    /** xlen the regfile bitWid */
    val xlen = 64
    /** fetch Ibus the pc bitWid  */
    val pcWid = 40
    /** fetch addr default value */
    val default_pc = 0x0
    /** the wid of the ifu fetch wid */
    val fetchWid = 128
}

trait L2Config{
    val l2addrwid = 40
    val l2datawid = 128
}

trait ICacheConfig{
    /** the wid of core fetch addr */
    val icacheAddrWid = GConfig.pcWid
    /** the wid of core fetch data output */
    val icacheDataWid = GConfig.fetchWid
    /**
     * the wid of cacheline_data bit
     * icache refill must as wide as cacline wid
     * */
    val icacheLineWid = icacheDataWid
    /** the number of cacheline */
    val lineNums = 16
    /** the number of cacheway */
    val wayNums = 8
    /** the wid of offset bit */
    val offsetWid = log2Up(icacheLineWid/8) //4
    /** the wid of index bit */
    val indexWid = log2Up(lineNums) // 2
    /** the wid of tag bit */
    val tagWid = icacheAddrWid - indexWid - offsetWid
    /** icache and ifu must have the same dataWid */
    assert(icacheDataWid == icacheLineWid)
}

trait DCacheConfig{
    /** the wid of dcache and memf addr */
    val dcacheAddrWid = GConfig.xlen
    /** the wid of dcache and memf data output */
    val dcacheDataWid = GConfig.xlen
    /**
     * the wid of dcacheline data
     * dCache refill must as wide as cacheline wid
    */
    val dcacheLineWid = 128
    /** the number of bank */
    val bankNums = 1
    /** the number of cacheline for each bank */
    val lineNums = 128 / bankNums
    /** the number of cacheway */
    val wayNums = 4
    /** the wid of offset bit */
    val offsetWid = log2Up(dcacheLineWid/8) //4
    /** the wid of index bit */
    val indexWid = log2Up(lineNums) // 2
    /** the wid of tag bit */
    val tagWid = dcacheAddrWid - indexWid - offsetWid

    /** dcache and memf can have the different dataWid */
    /** dcache and memf has the same dataWid */
    val issyncData = dcacheDataWid == dcacheLineWid
}

