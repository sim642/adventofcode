package eu.sim642.adventofcode2019

import Intcode._

object Day9 {

  def runBoost(program: Memory, input: Value): Value = {
    val outputs = ProgramState(program, LazyList(input)).outputs
    assert(outputs.lengthIs == 1)
    outputs.head
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(runBoost(parseProgram(input), 1))
    println(runBoost(parseProgram(input), 2))
  }
}
