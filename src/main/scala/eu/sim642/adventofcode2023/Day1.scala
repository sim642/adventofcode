package eu.sim642.adventofcode2023

object Day1 {

  trait Part {
    def recoverCalibrationValue(line: String): Int

    def sumCalibrationValues(document: Seq[String]): Int = document.map(recoverCalibrationValue).sum
  }

  object Part1 extends Part {
    def recoverCalibrationValue(line: String): Int = {
      val digits = line.filter(_.isDigit).map(_.asDigit)
      10 * digits.head + digits.last
    }
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

    private val map2: Map[String, Int] = for {
      (d, s) <- digitStrings
    } yield s -> d

    /*def recoverCalibrationValue(line: String): Int = {
      val line2 = digitStrings.foldLeft(line)({ case (line, (digit, string)) =>
        line.replaceAll(string, digit.toString)
      })
      println(Part1.recoverCalibrationValue(line2))
      Part1.recoverCalibrationValue(line2)
    }*/

    def recoverCalibrationValue(line: String): Int = {
      val first = digitStrings.values.filter(line.contains).map(line.indexOf).minOption
      val last = digitStrings.values.map(line.lastIndexOf).maxOption
      val digit1 = line.indexWhere(_.isDigit)
      val digit2 = line.lastIndexWhere(_.isDigit)
      val x =
        if (digit1 >= 0 && (first.isEmpty || digit1 < first.get))
          line(digit1).asDigit
        else {
          map2(digitStrings.values.filter(line.contains).minBy(line.indexOf))
        }
      val y =
        if (last.isEmpty || digit2 > last.get)
          line(digit2).asDigit
        else {
          map2(digitStrings.values.maxBy(line.lastIndexOf))
        }
      //println(10 * x + y)
      10 * x + y
    }
  }

  def parseDocument(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumCalibrationValues(parseDocument(input)))
    println(Part2.sumCalibrationValues(parseDocument(input)))
  }
}
