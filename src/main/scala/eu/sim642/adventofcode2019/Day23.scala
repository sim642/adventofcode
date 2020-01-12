package eu.sim642.adventofcode2019

import intcode._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day23 {

  case class Packet(x: Value, y: Value) {
    def toInput: LazyList[Value] = LazyList(x, y)
  }

  case class Computer(programState: ProgramState, inQueue: Queue[Packet])

  def runNetwork(program: Memory): Value = {

    var iter = 0
    @tailrec
    def helper(computers: Vector[Computer]): Value = {
      println(s"Iteration $iter:")
      iter += 1

      val (newComputers, outPackets) = computers.zipWithIndex.map({
        case (Computer(programState, inQueue), i) =>
          //print(s"Running $i... ")
          val inputs = {
            if (inQueue.nonEmpty)
              inQueue.head.toInput
            else
              LazyList(-1L)
            //(inQueue.flatMap(_.toInput) :+ -1L).to(LazyList)
          }

          val (newProgramState, outPacket) = programState.copy(inputs = inputs).outputStates.take(3) match {
            case LazyList() => // halted
              //println("no output")
              //(programState, None)
              (programState.copy(inputs = inputs).execs.last._1, None) // TODO: why is this execs.last necessary?
            case LazyList((_, outAddress), (_, outX), (newProgramState, outY)) =>
              println(s"$i -> $outAddress $outX $outY")
              (newProgramState, Some((outAddress.toInt, Packet(outX, outY))))
          }

          val newInQueue = {
            if (inQueue.nonEmpty && newProgramState.inputs.isEmpty)
              inQueue.tail
            else
              inQueue
            //inQueue.takeRight((newProgramState.inputs.size - 1) / 2)
          }
          val newComputer = Computer(newProgramState, newInQueue)
          (newComputer, outPacket)
      }).unzip

      outPackets.flatten.find(_._1 == 255) match {
        case Some((_, packet)) =>
          packet.y
        case None =>
          val newComputers2 = outPackets.flatten.foldLeft(newComputers)({ case (newComputers, (i, packet)) =>
            println(i, packet)
            val computerI = newComputers(i)
            val newComputerI = computerI.copy(inQueue = computerI.inQueue :+ packet)
            newComputers.updated(i, newComputerI)
          })

          helper(newComputers2)
      }
    }

    val computers = Vector.tabulate(50)({ i =>
      Computer(ProgramState(program, inputs = LazyList(i)).execOne.get._1, Queue.empty)
    })

    helper(computers)
  }

  def runNetworkNat(program: Memory): Value = {

    var iter = 0
    @tailrec
    def helper(computers: Vector[Computer], natPacket: Option[Packet], prevNatY: Option[Value]): Value = {
      println(s"Iteration $iter:")
      iter += 1

      val (newComputers, outPackets) = computers.zipWithIndex.map({
        case (Computer(programState, inQueue), i) =>
          //print(s"Running $i... ")
          val inputs = {
            if (inQueue.nonEmpty)
              inQueue.head.toInput
            else
              LazyList(-1L)
            //(inQueue.flatMap(_.toInput) :+ -1L).to(LazyList)
          }

          val (newProgramState, outPacket) = programState.copy(inputs = inputs).outputStates.take(3) match {
            case LazyList() => // halted
              //println("no output")
              //(programState, None)
              (programState.copy(inputs = inputs).execs.last._1, None) // TODO: why is this execs.last necessary?
            case LazyList((_, outAddress), (_, outX), (newProgramState, outY)) =>
              println(s"$i -> $outAddress $outX $outY")
              (newProgramState, Some((outAddress.toInt, Packet(outX, outY))))
          }

          val newInQueue = {
            if (inQueue.nonEmpty && newProgramState.inputs.isEmpty)
              inQueue.tail
            else
              inQueue
            //inQueue.takeRight((newProgramState.inputs.size - 1) / 2)
          }
          val newComputer = Computer(newProgramState, newInQueue)
          (newComputer, outPacket)
      }).unzip

      val outPackets2 = outPackets.flatten

      val idle = outPackets2.isEmpty && newComputers.forall(computer => computer.inQueue.isEmpty && computer.programState.opcode == 3) // TODO: remove opcode hack
      val (outPackets3, newPrevNatY) = {
        if (idle) {
          println("IDLE")
          val packet@Packet(_, natY) = natPacket.get
          if (prevNatY.contains(natY))
            return natY // TODO: remove return hack

          (Vector((0, packet)), Some(natY))
        }
        else
          (outPackets2, prevNatY)
      }

      val (newComputers2, newNatPacket) = outPackets3.foldLeft((newComputers, natPacket))({ case ((newComputers, newNatPacket), (i, packet)) =>
        println(i, packet)
        if (i == 255) {
          (newComputers, Some(packet))
        }
        else {
          val computerI = newComputers(i)
          val newComputerI = computerI.copy(inQueue = computerI.inQueue :+ packet)
          (newComputers.updated(i, newComputerI), newNatPacket)
        }
      })

      helper(newComputers2, newNatPacket, newPrevNatY)
    }

    val computers = Vector.tabulate(50)({ i =>
      Computer(ProgramState(program, inputs = LazyList(i)).execOne.get._1, Queue.empty)
    })

    helper(computers, None, None)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(runNetwork(parseProgram(input)))
    println(runNetworkNat(parseProgram(input)))
  }
}
