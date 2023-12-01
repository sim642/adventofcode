package eu.sim642.adventofcode2023

object Day1 {

  def recoverCalibrationValue(line: String): Int = {
    val digits = line.filter(_.isDigit).map(_.asDigit)
    10 * digits.head + digits.last
  }

  def sumCalibrationValues(document: Seq[String]): Int = document.map(recoverCalibrationValue).sum

  def parseDocument(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumCalibrationValues(parseDocument(input)))
  }
}
