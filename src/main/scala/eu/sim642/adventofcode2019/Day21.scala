package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._

object Day21 {

  def hullDamage(program: Memory): Int = {
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
    println(outputString)

    outputs.last.toInt
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(hullDamage(parseProgram(input)))
  }
}
