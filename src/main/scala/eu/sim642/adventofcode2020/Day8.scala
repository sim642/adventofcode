package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

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
  }
}
