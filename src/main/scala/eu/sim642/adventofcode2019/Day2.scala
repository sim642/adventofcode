package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._
import intcode._

object Day2 {

  def execNounVerb(program: Memory, noun: Int = 12, verb: Int = 2): Int = {
    val initialMemory = program.updated(1, noun.toLong).updated(2, verb.toLong)
    val finalMemory = ProgramState(initialMemory).execFinal.memory
    finalMemory(0).toInt
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execNounVerb(parseProgram(input)))
    println(findNounVerb(parseProgram(input)))
  }
}
