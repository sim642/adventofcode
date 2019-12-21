package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._

object Day21 {

  def hullDamageWalk(program: Memory): Int = {
    val inputString =
      s"""NOT C J
         |AND D J
         |NOT A T
         |OR T J
         |WALK
         |""".stripMargin
    val inputs = inputString.map(_.toLong).to(LazyList)

    val outputs = ProgramState(program, inputs = inputs).outputs
    val outputString = outputs.map(_.toChar).mkString
    Console.err.println(outputString)

    outputs.last.toInt
  }

  def hullDamageRun(program: Memory): Int = {
    val inputString =
      s"""NOT A J
         |NOT C T
         |OR T J
         |NOT B T
         |OR T J
         |AND D J
         |NOT D T
         |OR E T
         |OR H T
         |AND T J
         |RUN
         |""".stripMargin
    val inputs = inputString.map(_.toLong).to(LazyList)

    val outputs = ProgramState(program, inputs = inputs).outputs
    val outputString = outputs.map(_.toChar).mkString
    Console.err.println(outputString)

    outputs.last.toInt
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(hullDamageWalk(parseProgram(input)))
    println(hullDamageRun(parseProgram(input)))
  }
}
