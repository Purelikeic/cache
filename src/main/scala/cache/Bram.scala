package cache

import chisel3._
import chisel3.util._

class CacheDualPortBRAM extends Module {
  val io = IO(new Bundle {
    // write port
    val wr_en = Input(Bool())
    val wr_index = Input(UInt(6.W))
    val wr_data = Input(UInt(64.W))

    // read port
    val rd_en = Input(Bool())
    val rd_index = Input(UInt(6.W))
    val rd_data = Output(UInt(64.W))
  })

  // data memory
  val data_mem = SyncReadMem(64, UInt(64.W))

  // write op
  when (io.wr_en) {
    data_mem.write(io.wr_index, io.wr_data)
  }

  // read op
  io.rd_data := data_mem.read(io.rd_index, io.rd_en)
}
