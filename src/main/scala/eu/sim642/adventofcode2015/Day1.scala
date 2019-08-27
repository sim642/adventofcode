package eu.sim642.adventofcode2015

object Day1 {

  def finalFloor(instructions: String): Int = {
    instructions.foldLeft(0)({ case (floor, instruction) =>
      instruction match {
        case '(' => floor + 1
        case ')' => floor - 1
        case _ => ???
      }
    })
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(finalFloor(input))
  }
}
