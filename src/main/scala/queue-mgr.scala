package hurroccane

import Chisel._
import uncore._
import HurroccaneConstants._ // until params() is working in project

class HurroccanePort extends Bundle with UsesHurroccaneParameters {
  val in = Decoupled(Bits(width = HurroccaneConstants.XprLen)).flip
  val out = Decoupled(Bits(width = HurroccaneConstants.XprLen))
}

class HurroccaneQueueMgrCmd extends Bundle with UsesHurroccaneParameters {
  val op       = Bits(width = log2Up(HurroccaneConstants.ops.size))
  val recvDir  = Bits(width = log2Up(HurroccaneConstants.HurroccaneNumPorts))
  val sendMask = Bits(width = HurroccaneConstants.HurroccaneNumPorts)
  val sendData = Bits(width = HurroccaneConstants.XprLen)
}

class HurroccaneQueueMgrResp extends Bundle with UsesHurroccaneParameters {
  val respData = Bits(width = HurroccaneConstants.XprLen)
}

class HurroccaneQueueMgrIO extends Bundle with UsesHurroccaneParameters {
  val ports = Vec.fill(HurroccaneConstants.HurroccaneNumPorts){ new HurroccanePort() }
  val cmd   = Decoupled(new HurroccaneQueueMgrCmd).flip
  val resp  = Decoupled(new HurroccaneQueueMgrResp)
}

class HurroccaneQueueMgr extends Module with UsesHurroccaneParameters {
  val io = new HurroccaneQueueMgrIO

  val isRecv  = ((io.cmd.bits.op === HurroccaneConstants.ops("recv"))
                 || (io.cmd.bits.op === HurroccaneConstants.ops("nbrecv")))
  val inDir   = io.cmd.bits.recvDir

  val isSend  = ((io.cmd.bits.op === HurroccaneConstants.ops("send"))
                 || (io.cmd.bits.op === HurroccaneConstants.ops("nbsend")))
  val outMask = Mux(isSend,
                    io.cmd.bits.sendMask,
                    Bits(0))

  val isNonBlocking = ((io.cmd.bits.op === HurroccaneConstants.ops("nbsend"))
                       || (io.cmd.bits.op === HurroccaneConstants.ops("nbrecv")))

  val isLoad = io.cmd.bits.op === HurroccaneConstants.ops("ldnb")

  val nbrecv_r = Reg(Bits(width = HurroccaneConstants.XprLen))

  // examine global readiness
  val outPortClear = Vec.fill(HurroccaneConstants.HurroccaneNumPorts){ Bool() }
  for (i <- 0 until HurroccaneConstants.HurroccaneNumPorts) {
    outPortClear(i) := io.ports(i).out.ready || ~outMask(i)
  }
  val inPortClear = io.ports(inDir).in.valid || ~isRecv
  val allClear = ((~outPortClear.toBits()) === Bits(0)) && inPortClear

  // setup input ports
  for (i <- 0 until HurroccaneConstants.HurroccaneNumPorts) {
    io.ports(i).in.ready := Bool(false)
  }
  io.ports(inDir).in.ready := allClear && isSend

  // setup output ports
  val blockedOn = Vec.fill(HurroccaneConstants.HurroccaneNumPorts){ Bool() }
  for (i <- 0 until HurroccaneConstants.HurroccaneNumPorts) {
    blockedOn(i) := Bool(false)
    for (j <- 0 until HurroccaneConstants.HurroccaneNumPorts) {
      if (i != j) {
        blockedOn(j) := ~io.ports(j).out.ready && outMask(j)
      }
    }
    io.ports(i).out.valid := (blockedOn.toBits() === Bits(0)) && outMask(i)
    io.ports(i).out.bits := io.cmd.bits.sendData
  }

  // setup cmd ready
  io.cmd.ready := io.resp.ready && (allClear || isNonBlocking)

  // setup resp
  io.resp.valid := io.cmd.valid && (allClear || isNonBlocking)
  io.resp.bits.respData := io.ports(inDir).in.bits
  when (isNonBlocking && isSend) {
    io.resp.bits.respData := blockedOn.toBits()
  }
  when (isNonBlocking && isRecv) {
    io.resp.bits.respData := ~io.ports(inDir).in.valid
    when (allClear) {
      nbrecv_r := io.ports(inDir).in.bits
    }
  }
  when (isLoad) {
    io.resp.bits.respData := nbrecv_r
  }
}


