package eu.sim642.adventofcode2024

object Day7 {

  case class Equation(testValue: Long, numbers: Seq[Int])

  trait Part {
    def isSolvable(equation: Equation): Boolean

    def totalCalibrationResult(equations: Seq[Equation]): Long = {
      equations
        .filter(isSolvable)
        .map(_.testValue)
        .sum
    }
  }

  object Part1 extends Part {
    override def isSolvable(equation: Equation): Boolean = {

      def helper(testValue: Long, numbers: List[Int]): Boolean = numbers match {
        case Nil => ???
        case List(number) => number == testValue
        case number :: numbers =>
          testValue % number == 0 && helper(testValue / number, numbers) || helper(testValue - number, numbers)
      }

      helper(equation.testValue, equation.numbers.view.reverse.toList)
    }
  }

  object Part2 extends Part {
    override def isSolvable(equation: Equation): Boolean = {

      def helper(testValue: Long, numbers: List[Int]): Boolean = numbers match {
        case Nil => ???
        case List(number) => number == testValue
        case number :: newNumbers =>
          testValue % number == 0 && helper(testValue / number, newNumbers) || helper(testValue - number, newNumbers) || concatHelper(testValue, numbers)
      }

      def concatHelper(testValue: Long, numbers: List[Int]): Boolean = numbers match {
        case number :: newNumbers =>
          val testValueStr = testValue.toString
          val numberStr = number.toString
          if (testValueStr.length > numberStr.length && testValueStr.endsWith(numberStr)) {
            val newTestValue = testValueStr.dropRight(numberStr.length).toLong
            helper(newTestValue, newNumbers)
          }
          else
            false
        case _ => false
      }

      helper(equation.testValue, equation.numbers.view.reverse.toList)
    }
  }

  def parseEquation(s: String): Equation = s match {
    case s"$testValue: $numbers" =>
      Equation(testValue.toLong, numbers.split(" ").map(_.toInt).toSeq)
  }

  def parseInput(input: String): Seq[Equation] = input.linesIterator.map(parseEquation).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalCalibrationResult(parseInput(input)))
    println(Part2.totalCalibrationResult(parseInput(input)))
  }
}
