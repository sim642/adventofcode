package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day2 {

  type Memory = Vector[Int]

  case class ProgramState(memory: Memory, ip: Int = 0) {
    def instruction(i: Int): Int = memory(ip + i)
    def opcode: Int = instruction(0)
    def param(i: Int): Int = instruction(i + 1)

    def execOne: Option[ProgramState] = {
      opcode match {
        case 1 =>
          val newValue = memory(param(0)) + memory(param(1))
          val newMemory = memory.updated(param(2), newValue)
          Some(ProgramState(newMemory, ip + 4))
        case 2 =>
          val newValue = memory(param(0)) * memory(param(1))
          val newMemory = memory.updated(param(2), newValue)
          Some(ProgramState(newMemory, ip + 4))
        case 99 => None
        case _ => throw new IllegalArgumentException("Unknown opcode")
      }
    }

    def execIterator: Iterator[ProgramState] = {
      Iterator.unfold0(this)(_.execOne)
    }

    def execFinal: ProgramState = {
      execIterator.last
    }
  }

  def execNounVerb(program: Memory, noun: Int = 12, verb: Int = 2): Int = {
    val initialMemory = program.updated(1, noun).updated(2, verb)
    val finalMemory = ProgramState(initialMemory).execFinal.memory
    finalMemory(0)
  }

  def findNounVerb(program: Memory, requiredOutput: Int = 19690720): Int = {
    val (noun, verb) = (for {
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
      output = execNounVerb(program, noun, verb)
      if output == requiredOutput
    } yield (noun, verb)).head

    100 * noun + verb
  }

  def parseProgram(input: String): Memory = input.split(',').toVector.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execNounVerb(parseProgram(input)))
    println(findNounVerb(parseProgram(input)))
  }
}
