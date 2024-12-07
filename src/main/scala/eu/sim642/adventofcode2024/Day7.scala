package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IntegralImplicits._

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

  def unAdd(testValue: Long, number: Int): Option[Long] =
    if (testValue > number) Some(testValue - number) else None

  def unMultiply(testValue: Long, number: Int): Option[Long] = testValue /! number

  def unConcatenate(testValue: Long, number: Int): Option[Long] = {
    val testValueStr = testValue.toString
    val numberStr = number.toString
    if (testValueStr.length > numberStr.length && testValueStr.endsWith(numberStr))
      Some(testValueStr.dropRight(numberStr.length).toLong)
    else
      None
  }

  trait RightToLeftPart extends Part {
    val unOperators: Seq[(Long, Int) => Option[Long]]

    override def isSolvable(equation: Equation): Boolean = {

      def helper(testValue: Long, numbers: List[Int]): Boolean = numbers match {
        case Nil => throw new IllegalAccessException("illegal empty equation")
        case List(number) => number == testValue
        case number :: newNumbers =>
          unOperators.exists(_(testValue, number).exists(helper(_, newNumbers)))
      }

      helper(equation.testValue, equation.numbers.view.reverse.toList)
    }
  }

  object Part1 extends RightToLeftPart {
    override val unOperators: Seq[(Long, Int) => Option[Long]] = Seq(unMultiply, unAdd)
  }

  object Part2 extends RightToLeftPart {
    override val unOperators: Seq[(Long, Int) => Option[Long]] = Seq(unMultiply, unAdd, unConcatenate)
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
