package eu.sim642.adventofcode2024

object Day7 {

  case class Equation(testValue: Long, numbers: Seq[Int])

  def isSolvable(equation: Equation): Boolean = {

    def helper(testValue: Long, numbers: List[Int]): Boolean = numbers match {
      case Nil => ???
      case List(number) => number == testValue
      case number :: numbers =>
        testValue % number == 0 && helper(testValue / number, numbers) || helper(testValue - number, numbers)
    }

    helper(equation.testValue, equation.numbers.view.reverse.toList)
  }

  def totalCalibrationResult(equations: Seq[Equation]): Long = {
    equations
      .filter(isSolvable)
      .map(_.testValue)
      .sum
  }

  def parseEquation(s: String): Equation = s match {
    case s"$testValue: $numbers" =>
      Equation(testValue.toLong, numbers.split(" ").map(_.toInt).toSeq)
  }

  def parseInput(input: String): Seq[Equation] = input.linesIterator.map(parseEquation).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalCalibrationResult(parseInput(input)))
  }
}
