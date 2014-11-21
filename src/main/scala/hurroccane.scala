package hurroccane

import Chisel._
import uncore._

object HurroccaneConstants {
  val ops = Map(
                "send" -> Bits("b0"),
                "recv" -> Bits("b1")
               )
}

abstract trait UsesHurroccaneParameters extends UsesParameters {

}

class Hurroccane(isLoopback: Boolean = true)
extends rocket.RoCC with UsesHurroccaneParameters {

  // Queue manager is the "core logic" of the hurroccane rocc queues
  val queueMgr = Module(new HurroccaneQueueMgr())


  // Command shim logic
  queueMgr.io.cmd.valid := io.cmd.valid
  queueMgr.io.cmd.bits.op := io.cmd.bits.inst.funct(log2Up(HurroccaneConstants.ops.size)-1,0)
  queueMgr.io.cmd.bits.recvDir := io.cmd.bits.rs1(log2Up(params(HurroccaneNumPorts))-1,0)
  queueMgr.io.cmd.bits.sendMask := io.cmd.bits.rs1(params(HurroccaneNumPorts)-1,0)
  queueMgr.io.cmd.bits.sendData := io.cmd.bits.rs2
  io.cmd.ready := queueMgr.io.cmd.ready


  // Response shim logic
  io.resp.valid := queueMgr.io.resp.valid
  io.resp.bits.rd := io.cmd.bits.rd
  io.resp.bits.data := queueMgr.io.resp.bits.recvData
  queueMgr.io.resp.ready := io.resp.ready


  // Loopback queues for testing
  if (isLoopback) {
    val loopbackQueues = Vec.fill(params(HurroccaneNumPorts)) {
      Queue(Bits(width = params(XprLen))), 2)
    }
    for (i <- 0 until params(HurroccaneNumPorts)) {
      queueMgr.io.ports(i).out <> loopbackQueues.io.enq
      loopbackQueues.io.deq    <> queueMgr.io.ports(i).in
    }
  }
}
