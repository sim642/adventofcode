package eu.sim642.adventofcode2019

import Intcode._

object Day5 {

  def execInputsReverse(program: Memory, inputs: LazyList[Int]): LazyList[Int] = {
    // TODO: get rid of this useless method
    ProgramState(program, inputs.map(_.toLong)).outputs.map(_.toInt).reverse
  }

  def execDiagnostic(program: Memory, systemId: Int): Int = {
    val (diagnostic #:: outputs) = execInputsReverse(program, LazyList(systemId))
    assert(outputs.forall(_ == 0))
    diagnostic
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execDiagnostic(parseProgram(input), 1))
    println(execDiagnostic(parseProgram(input), 5))
  }
}
