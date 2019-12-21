package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._

object Day21 {

  trait Part {
    val inputString: String

    def hullDamage(program: Memory): Int = {
      val inputs = inputString.map(_.toLong).to(LazyList)

      val outputs = ProgramState(program, inputs = inputs).outputs
      val outputString = outputs.map(_.toChar).mkString
      Console.err.println(outputString)

      outputs.last.toInt
    }
  }

  object Part1 extends Part {
    // (-C & D) | -A
    override val inputString: String =
      """NOT C J
         |AND D J
         |NOT A T
         |OR T J
         |WALK
         |""".stripMargin
  }

  object Part2 extends Part {
    // (-A | -C | -B) & D & (E | H)
    override val inputString: String =
      """NOT A J
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
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.hullDamage(parseProgram(input)))
    println(Part2.hullDamage(parseProgram(input)))
  }
}
