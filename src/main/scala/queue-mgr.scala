package hurroccane

import Chisel._
import uncore._

class HurroccanePort extends Bundle with UsesHurroccaneParameters {
  val in = Decoupled(Bits(width = params(XprLen))).flip
  val out = Decoupled(Bits(width = params(XprLen)))
}

class HurroccaneQueueMgrCmd extends Bundle with UsesHurroccaneParameters {
  val op       = Bits(width = log2Up(HurroccaneConstants.ops.size))
  val recvDir  = Bits(width = log2Up(params(HurroccaneNumPorts)))
  val sendMask = Bits(width = log2Up(params(HurroccaneNumPorts)))
  val sendData = Bits(width = params(XprLen))
}

class HurroccaneQueueMgrResp extends Bundle with UsesHurroccaneParameters {
  val recvData = Bits(width = params(XprLen))
}

class HurroccaneQueueMgrIO extends Bundle with UsesHurroccaneParameters {
  val ports = Vec.fill(params(HurroccaneNumPorts)){ new HurroccanePort() }
  val cmd   = Decoupled(HurroccaneQueueMgrCmd).flip
  val resp  = Decoupled(HurroccaneQueueMgrResp)
}

class HurroccanneQueueMgr extends Module with UsesHurroccaneParameters {
  val io = new HurroccaneQueueMgrIO

  val isRecv  = io.cmd.bits.op == HurroccaneConstants.ops("recv")
  val inDir   = io.cmd.bits.recvDir

  val isSend = io.cmd.bits.op == HurroccaneConstants.ops("send")
  val outMask = Mux(isSend,
                    io.cmd.bits.sendMask,
                    Bits(0))

  // examine global readiness
  val outPortClear = Vec.fill(params(HurroccaneNumPorts)){ Bool() }
  for (i <- 0 until params(HurroccaneNumPorts)) {
    outPortClear(i) = io.ports(i).out.ready || ~outMask(i)
  }
  val inPortClear  = io.ports(inDir).in.ready || ~isRecv
  val allClear = outPortClear.toBits().AndR && inPortClear

  // setup input ports
  for (i <- 0 until params(HurroccaneNumPorts)) {
    io.ports(i).in.ready := Bool(false)
  }
  io.ports(inDir).in.ready := allClear

  // setup output ports
  for (i <- 0 until params(HurroccaneNumPorts)) {
    val blockedOn = Vec.fill(params(HurroccaneNumPorts)){ Bool() }
    blockedOn(i) := Bool(false)
    for (j <- 0 until params(HurroccaneNumPorts)) {
      if (i != j) {
        blockedOn(j) := ~io.ports(j).out.ready && outMask(j))
      }
    }
    io.ports(i).out.valid := (blockedOn.toBits() == Bits(0))
    io.ports(i).out.bits := io.cmd.bits.sendData
  }

  // setup cmd ready
  io.cmd.ready := io.resp.ready && allClear

  // setup resp
  io.resp.valid := io.cmd.valid && allClear
  io.resp.bits.recvData := io.ports(inDir).in.bits
}


