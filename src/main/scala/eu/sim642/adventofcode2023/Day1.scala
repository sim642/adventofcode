package eu.sim642.adventofcode2023

object Day1 {

  trait Part {
    def recoverDigits(line: String): Seq[Int]

    def recoverCalibrationValue(line: String): Int = {
      val digits = recoverDigits(line)
      10 * digits.head + digits.last
    }

    def sumCalibrationValues(document: Seq[String]): Int = document.map(recoverCalibrationValue).sum
  }

  object Part1 extends Part {
    override def recoverDigits(line: String): Seq[Int] =
      line.filter(_.isDigit).map(_.asDigit) // TODO: asDigitOpt in library
  }

  object Part2 extends Part {

    private val digitStrings = Map(
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
    )

    override def recoverDigits(line: String): Seq[Int] = {
      line.tails.flatMap(tail =>
        tail.headOption match {
          case Some(c) if c.isDigit => Some(c.asDigit)
          case _ =>
            digitStrings.find((_, digitString) => tail.startsWith(digitString)).map(_._1)
        }
      ).toSeq
    }
  }

  def parseDocument(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumCalibrationValues(parseDocument(input)))
    println(Part2.sumCalibrationValues(parseDocument(input)))
  }
}
