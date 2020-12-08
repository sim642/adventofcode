package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day8 {

  sealed trait Instruction
  case class Acc(arg: Int) extends Instruction
  case class Jmp(arg: Int) extends Instruction
  case class Nop(arg: Int) extends Instruction

  type Instructions = Vector[Instruction]

  case class ProgramState(instructions: Instructions, ip: Int = 0, acc: Int = 0) {
    def execOne: ProgramState = instructions(ip) match {
      case Acc(arg) =>
        copy(ip = ip + 1, acc = acc + arg)
      case Jmp(arg) =>
        copy(ip = ip + arg)
      case Nop(arg) =>
        copy(ip = ip + 1)
    }
  }

  def accBeforeLoop(instructions: Instructions): Int = {
    val programState = ProgramState(instructions)
    // TODO: refactor findBy: better type inference, indexing
    val cycle = NaiveCycleFinder.findBy(programState, (_: ProgramState).execOne)(_.ip)
    cycle.cycleHeadRepeat.acc
  }

  def accAfterFix(instructions: Instructions): Int = {
    def result(instructions: Instructions): Option[Int] = {
      val programState = ProgramState(instructions)
      // TODO: refactor cycle finding to handle non-existent cycle
      try {
        val cycle = NaiveCycleFinder.findBy(programState, (_: ProgramState).execOne)(_.ip)
        None
      } catch {
        case _: IndexOutOfBoundsException =>
          //println("index")
          val last = Iterator.iterate(programState)(_.execOne).takeWhile(ps => ps.instructions.indices.contains(ps.ip)).last
          val last2 = last.execOne
          //println(last2)
          if (last2.ip == last2.instructions.size)
            Some(last2.acc)
          else
            None
      }
    }

    instructions.indices.flatMap({ i =>
      instructions(i) match {
        case Acc(arg) => None
        case Jmp(arg) => Some(instructions.updated(i, Nop(arg)))
        case Nop(arg) => Some(instructions.updated(i, Jmp(arg)))
      }
    }).flatMap(result).head
  }

  private val instructionRegex = """([a-z]+) ([+-]\d+)""".r

  def parseInstruction(s: String): Instruction = s match {
    case instructionRegex(instruction, arg) => instruction match {
      case "acc" => Acc(arg.toInt)
      case "jmp" => Jmp(arg.toInt)
      case "nop" => Nop(arg.toInt)
    }
  }

  def parseInstructions(input: String): Instructions = input.linesIterator.map(parseInstruction).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(accBeforeLoop(parseInstructions(input)))
    println(accAfterFix(parseInstructions(input)))
  }
}
