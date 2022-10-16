package eu.sim642.adventofcode2020

import Day8.Instruction._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.LazyListImplicits._

object Day8 {

  enum Instruction {
    case Acc(arg: Int)
    case Jmp(arg: Int)
    case Nop(arg: Int)
  }

  type Instructions = Vector[Instruction]

  case class ProgramState(instructions: Instructions, ip: Int = 0, acc: Int = 0) {
    def execOne: Option[ProgramState] = {
      if (instructions.indices.contains(ip))
        Some(instructions(ip) match {
          case Acc(arg) =>
            copy(ip = ip + 1, acc = acc + arg)
          case Jmp(arg) =>
            copy(ip = ip + arg)
          case Nop(arg) =>
            copy(ip = ip + 1)
        })
      else
        None
    }

    def execs: LazyList[ProgramState] = LazyList.unfold0(this)(_.execOne)
  }

  def accBeforeLoop(instructions: Instructions): Int = {
    val programState = ProgramState(instructions)
    // TODO: refactor findBy: better type inference, indexing
    val cycle = NaiveCycleFinder.findBy(programState.execs)(_.ip).get
    cycle.cycleHeadRepeat.acc
  }

  def accAfterFix(instructions: Instructions): Int = {
    def result(instructions: Instructions): Option[Int] = {
      val programState = ProgramState(instructions)
      val execs = programState.execs
      NaiveCycleFinder.findBy(execs)(_.ip) match {
        case Some(_) => None
        case None =>
          val last = execs.last
          if (last.ip == last.instructions.size) // required by text but actually unnecessary
            Some(last.acc)
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
