package cache

import chisel3._
import chisel3.util._

/** 48-bit address is divided into 3 parts: Tag, Index and Offset.
  * The width of each part is determined by the cache configuration.
  * +-------+-------+--------+
  * |  Tag  | Index | Offset |
  * +-------+-------+--------+
  * - Tag: 36 bits
  * - Index: 6 bits (64 sets)
  * - Offset: 6 bits (64 bytes per block)
  */

class CacheTag(
  INDEX_WD: Int, // Index width = 6
  TAG_WD: Int, // Tag width = 36
  OFFSET_WD: Int // offset width = 6
) extends Module {
  val io = IO(new Bundle {
    /** TagModule accpets the new current request
      * and the address of the particular request.
      */
    val req = Input(Bool())
    val req_addr = Input(UInt(48.W))

    /* TagModule outputs the hit/miss signal */
    val hit = Output(UInt(4.W)) // hit which way
    val miss = Output(Bool())

    /** After receiving data from the RAM
      * update the corresponding input information for the exsiting Tag.
      */
    val replace_cache = Input(Bool()) // write enable for Cache
    val replace_addr = Input(UInt(48.W)) // the address of the new data
    /* The replacement of which way is computed within this module. */
    val replace_pointer = Output(UInt(4.W))
  })

  require(INDEX_WD + TAG_WD + OFFSET_WD == 48, "The sum of INDEX_WD, TAG_WD and OFFSET_WD should be 48.")

  val INDEX_NUM = 1 << INDEX_WD // number of sets
  val replacePointer = Wire(UInt(4.W)) // 4 bits to represent which way to replace (use ont-hot)
  io.replace_pointer := replacePointer

  val replace_cache = io.replace_cache

  val OFFSET_LSB = 0
  val OFFSET_MSB = OFFSET_WD - 1

  val INDEX_LSB = OFFSET_WD
  val INDEX_MSB = OFFSET_WD + INDEX_WD - 1

  val TAG_LSB = INDEX_MSB + 1
  val TAG_MSB = TAG_LSB + TAG_WD - 1

  /* On req arrival cycle: first steps - begin */
  // 1. Extract the address into Tag, Index and Offset
  val req = io.req
  val req_offset = io.req_addr(OFFSET_MSB, OFFSET_LSB)
  val req_index = io.req_addr(INDEX_MSB, INDEX_LSB)
  val req_tag = io.req_addr(TAG_MSB, TAG_LSB)
  // 2. Use index to find the corresponding set and compare the tag with the tags in the set
  val validTag = Cat(1.U(1.W), req_tag)
  // 2.1 way0
  val tag_way0 = RegInit(VecInit(Seq.fill(INDEX_NUM)(0.U((TAG_WD + 1).W))))
  val hit_way0 = req && (validTag === tag_way0(req_index))
  // 2.2 way1
  val tag_way1 = RegInit(VecInit(Seq.fill(INDEX_NUM)(0.U((TAG_WD + 1).W))))
  val hit_way1 = req && (validTag === tag_way1(req_index))
  // 2.3 way2
  val tag_way2 = RegInit(VecInit(Seq.fill(INDEX_NUM)(0.U((TAG_WD + 1).W))))
  val hit_way2 = req && (validTag === tag_way2(req_index))
  // 2.4 way3
  val tag_way3 = RegInit(VecInit(Seq.fill(INDEX_NUM)(0.U((TAG_WD + 1).W))))
  val hit_way3 = req && (validTag === tag_way3(req_index))

  val hit = Cat(hit_way3, hit_way2, hit_way1, hit_way0)
  val miss = req && ~(hit_way0 || hit_way1 || hit_way2 || hit_way3)
  io.hit := hit
  io.miss := miss

  /* tag Memory */
  val replace_offset = io.replace_addr(OFFSET_MSB, OFFSET_LSB)
  val replace_index = io.replace_addr(INDEX_MSB, INDEX_LSB)
  val replace_tag = io.replace_addr(TAG_MSB, TAG_LSB)

  when (replace_cache && replacePointer(0)) {
    tag_way0(replace_index) := Cat(1.U(1.W), replace_tag)
  }

  when (replace_cache && replacePointer(1)) {
    tag_way1(replace_index) := Cat(1.U(1.W), replace_tag)
  }

  when (replace_cache && replacePointer(2)) {
    tag_way2(replace_index) := Cat(1.U(1.W), replace_tag)
  }

  when (replace_cache && replacePointer(3)) {
    tag_way3(replace_index) := Cat(1.U(1.W), replace_tag)
  }

  /** Replace Algorithm - Tree-PLRU 
    * Tree-LRU:
              b2
            /    \
          b1      b0
         /  \    /  \
      way0 way1 way2 way3
   */
  val lru = RegInit(VecInit(Seq.fill(INDEX_NUM)(0.U(3.W)))) // 3 bits to represent the state of the tree-PLRU
  val use_index = Mux(hit.orR, req_index, replace_index)

  when (reset.asBool) {
    for (i <- 0 until INDEX_NUM) {
      lru(i) := 0.U
    }
  }
  .otherwise {
    when (hit_way0 || (replace_cache && replacePointer(0))) {
      lru(use_index) := Cat(
        1.U(1.W), // b2
        1.U(1.W), // b1
        lru(use_index)(0) // b0
      )
    }
    .elsewhen (hit_way1 || (replace_cache && replacePointer(1))) {
      lru(use_index) := Cat(
        1.U(1.W), // b2
        0.U(1.W), // b1
        lru(use_index)(0) // b0
      )
    }
    .elsewhen (hit_way2 || (replace_cache && replacePointer(2))) {
      lru(use_index) := Cat(
        0.U(1.W), // b2
        lru(use_index)(1), // b1
        1.U(1.W) // b0
      )
    }
    .elsewhen (hit_way3 || (replace_cache && replacePointer(3))) {
      lru(use_index) := Cat(
        0.U(1.W), // b2
        lru(use_index)(1), // b1
        0.U(1.W) // b0
      )
    }
  }
  val b2 = lru(replace_index)(2)
  val b1 = lru(replace_index)(1)
  val b0 = lru(replace_index)(0)
  replacePointer :=
    Mux(!b2,
      Mux(!b1, 1.U, 2.U), /* 0001: way0, 0010: way1 */
      Mux(!b0, 4.U, 8.U)  /* 0100: way2, 1000: way3 */
    )
}