package cache

import chisel3._
import chisel3.util._

class CacheData(
  INDEX_WD: Int,
  TAG_WD: Int,
  OFFSET_WD: Int
) extends Module {
  val io = IO(new Bundle {
    val req = Input(Bool())

    /* From outer side CacheCtrl's refill data */
    val replace_fromCtrl = Input(Bool())
    val refillAddr_fromCtrl = Input(UInt(64.W))
    val newCacheline_fromCtrl = Input(UInt(512.W))

    /* when hit read data */
    val hit_fromTag = Input(UInt(4.W))
    val reqAddr_fromCtrl = Input(UInt(64.W))
    val pointer_fromTag = Input(UInt(4.W))
    val rdata_toCtrl = Output(UInt(64.W))
  })

  val OFFSET_LSB = 0
  val OFFSET_MSB = OFFSET_WD - 1
  val INDEX_LSB = OFFSET_WD
  val INDEX_MSB = OFFSET_WD + INDEX_WD - 1
  val TAG_LSB = INDEX_MSB + 1
  val TAG_MSB = TAG_LSB + TAG_WD - 1

  /* Size of Cache block */
  // A cache block: 64B -> 8 * 64bit
  val CACHE_LINE = 1 << (OFFSET_WD - 3)

  // Calculate the refill infromation
  val refill_offset = io.refillAddr_fromCtrl(OFFSET_MSB, OFFSET_LSB)
  val refill_index = io.refillAddr_fromCtrl(INDEX_MSB, INDEX_LSB)
  val refill_tag = io.refillAddr_fromCtrl(TAG_MSB, TAG_LSB)

  // Calculate the req information
  val req_offset = io.reqAddr_fromCtrl(OFFSET_MSB, OFFSET_LSB)
  val req_index = io.reqAddr_fromCtrl(INDEX_MSB, INDEX_LSB)
  val req_tag = io.reqAddr_fromCtrl(TAG_MSB, TAG_LSB)

  // select data from cache block (512-bit choose 64bit)
  val bank_sel = 1.U << req_offset(5, 3) // use 5-3 to choose bank, and 2-0 to choose Byte

  // way0
  val Way0_brams = Seq.fill(CACHE_LINE)(Module(new CacheDualPortBRAM))
  val rdata_way0 = Wire(Vec(CACHE_LINE, UInt(64.W)))

  for (n <- 0 until CACHE_LINE) {
    // write signal
    val wr_en = io.replace_fromCtrl && io.pointer_fromTag(0)
    val wr_index = refill_index
    val startBit = n * 64
    val endBit = (n + 1) * 64 - 1
    val wr_data = io.newCacheline_fromCtrl(endBit, startBit)
    Way0_brams(n).io.wr_en := wr_en
    Way0_brams(n).io.wr_index := wr_index
    Way0_brams(n).io.wr_data := wr_data

    // read signal - single cycle read
    val rd_en = io.req && bank_sel(n) && io.hit_fromTag(0)
    val rd_index = req_index
    val rd_data = Way0_brams(n).io.rd_data
    Way0_brams(n).io.rd_en := rd_en
    Way0_brams(n).io.rd_index := rd_index
    rdata_way0(n) := rd_data
  }

  // way1
  val Way1_brams = Seq.fill(CACHE_LINE)(Module(new CacheDualPortBRAM))
  val rdata_way1 = Wire(Vec(CACHE_LINE, UInt(64.W)))

  for (n <- 0 until CACHE_LINE) {
    // write signal
    val wr_en = io.replace_fromCtrl && io.pointer_fromTag(1)
    val wr_index = refill_index
    val startBit = n * 64
    val endBit = (n + 1) * 64 - 1
    val wr_data = io.newCacheline_fromCtrl(endBit, startBit)
    Way1_brams(n).io.wr_en := wr_en
    Way1_brams(n).io.wr_index := wr_index
    Way1_brams(n).io.wr_data := wr_data

    // read signal - single cycle read
    val rd_en = io.req && bank_sel(n) && io.hit_fromTag(1)
    val rd_index = req_index
    val rd_data = Way1_brams(n).io.rd_data
    Way1_brams(n).io.rd_en := rd_en
    Way1_brams(n).io.rd_index := rd_index
    rdata_way1(n) := rd_data
  }

  // way2
  val Way2_brams = Seq.fill(CACHE_LINE)(Module(new CacheDualPortBRAM))
  val rdata_way2 = Wire(Vec(CACHE_LINE, UInt(64.W)))

  for (n <- 0 until CACHE_LINE) {
    // write signal
    val wr_en = io.replace_fromCtrl && io.pointer_fromTag(2)
    val wr_index = refill_index
    val startBit = n * 64
    val endBit = (n + 1) * 64 - 1
    val wr_data = io.newCacheline_fromCtrl(endBit, startBit)
    Way2_brams(n).io.wr_en := wr_en
    Way2_brams(n).io.wr_index := wr_index
    Way2_brams(n).io.wr_data := wr_data

    // read signal - single cycle read
    val rd_en = io.req && bank_sel(n) && io.hit_fromTag(2)
    val rd_index = req_index
    val rd_data = Way2_brams(n).io.rd_data
    Way2_brams(n).io.rd_en := rd_en
    Way2_brams(n).io.rd_index := rd_index
    rdata_way2(n) := rd_data
  }

  // way3
  val Way3_brams = Seq.fill(CACHE_LINE)(Module(new CacheDualPortBRAM))
  val rdata_way3 = Wire(Vec(CACHE_LINE, UInt(64.W)))

  for (n <- 0 until CACHE_LINE) {
    // write signal
    val wr_en = io.replace_fromCtrl && io.pointer_fromTag(3)
    val wr_index = refill_index
    val startBit = n * 64
    val endBit = (n + 1) * 64 - 1
    val wr_data = io.newCacheline_fromCtrl(endBit, startBit)
    Way3_brams(n).io.wr_en := wr_en
    Way3_brams(n).io.wr_index := wr_index
    Way3_brams(n).io.wr_data := wr_data

    // read signal - single cycle read
    val rd_en = io.req && bank_sel(n) && io.hit_fromTag(3)
    val rd_index = req_index
    val rd_data = Way3_brams(n).io.rd_data
    Way3_brams(n).io.rd_en := rd_en
    Way3_brams(n).io.rd_index := rd_index
    rdata_way3(n) := rd_data
  }

  // choose the desire 64bit
  val selected_way0_data = Wire(UInt(64.W))
  val selected_way1_data = Wire(UInt(64.W))
  val selected_way2_data = Wire(UInt(64.W))
  val selected_way3_data = Wire(UInt(64.W))

  selected_way0_data := Mux1H(bank_sel, rdata_way0)
  selected_way1_data := Mux1H(bank_sel, rdata_way1)
  selected_way2_data := Mux1H(bank_sel, rdata_way2)
  selected_way3_data := Mux1H(bank_sel, rdata_way3)

  val hit_selected_data = Wire(UInt(64.W))

  when (io.hit_fromTag(0)) {
    hit_selected_data := selected_way0_data
  }
  .elsewhen (io.hit_fromTag(1)) {
    hit_selected_data := selected_way1_data
  }
  .elsewhen (io.hit_fromTag(2)) {
    hit_selected_data := selected_way2_data
  }
  .elsewhen (io.hit_fromTag(3)) {
    hit_selected_data := selected_way3_data
  }
  .otherwise {
    hit_selected_data := 0.U // when miss
  }

  io.rdata_toCtrl := hit_selected_data
}