package hurroccane

import Chisel._
import uncore._

object HurroccaneConstants {
  val ops = Map(
                "send" -> Bits("b0"),
                "recv" -> Bits("b1")
               )
  def params(param: Int): Int = { param }
  val HurroccaneNumPorts: Int = 4
  val XprLen: Int = 64
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
  queueMgr.io.cmd.bits.recvDir := io.cmd.bits.rs1(log2Up(HurroccaneConstants.HurroccaneNumPorts)-1,0)
  queueMgr.io.cmd.bits.sendMask := io.cmd.bits.rs1(HurroccaneConstants.HurroccaneNumPorts-1,0)
  queueMgr.io.cmd.bits.sendData := io.cmd.bits.rs2
  io.cmd.ready := queueMgr.io.cmd.ready


  // Response shim logic
  io.resp.valid := queueMgr.io.resp.valid
  io.resp.bits.rd := io.cmd.bits.inst.rd
  io.resp.bits.data := queueMgr.io.resp.bits.recvData
  queueMgr.io.resp.ready := io.resp.ready


  // Loopback queues for testing
  if (isLoopback) {
    val loopbackQueues = Array.fill(HurroccaneConstants.HurroccaneNumPorts) {
      Module(new Queue(Bits(width = HurroccaneConstants.XprLen), 2))
    }
    for (i <- 0 until HurroccaneConstants.HurroccaneNumPorts) {
      loopbackQueues(i).io.enq <> queueMgr.io.ports(i).out
      queueMgr.io.ports(i).in <> loopbackQueues(i).io.deq
    }
  }

  
  // Tie off unused outputs
  io.busy := Bool(false)
  io.interrupt := Bool(false)

  io.mem.req.valid := Bool(false)
  io.mem.req.bits.addr := Bits(0)
  io.mem.req.bits.tag := Bits(0)
  io.mem.req.bits.cmd := Bits(0)
  io.mem.req.bits.typ := Bits(0)
  io.mem.req.bits.data := Bits(0)

  io.imem.acquire.valid := Bool(false)
  io.imem.grant.ready := Bool(false)
  io.imem.finish.valid := Bool(false)
  io.iptw.req.valid := Bool(false)
  io.dptw.req.valid := Bool(false)
  io.pptw.req.valid := Bool(false)

}
