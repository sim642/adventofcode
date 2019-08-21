package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.SeqImplicits._

object Day1 {

  trait Part {
    def captcha(digits: Seq[Int]): Int

    def captcha(digitsStr: String): Int = {
      val digits = digitsStr.toSeq.map(_.asDigit)
      captcha(digits)
    }
  }

  object Part1 extends Part {
    override def captcha(digits: Seq[Int]): Int =
      (digits, digits.rotateLeft(1)).zipped
        .map((digit, nextDigit) => if (digit == nextDigit) digit else 0)
        .sum
  }

  object Part2 extends Part {
    override def captcha(digits: Seq[Int]): Int = {
      require(digits.length % 2 == 0)

      (digits, digits.rotateLeft(digits.length / 2)).zipped
        .map((digit, nextDigit) => if (digit == nextDigit) digit else 0)
        .sum
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.captcha(input))
    println(Part2.captcha(input))
  }
}
