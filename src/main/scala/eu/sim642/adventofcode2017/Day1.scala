package eu.sim642.adventofcode2017

object Day1 {
  implicit class CyclicSeq[A](s: Seq[A]) {
    def cyclicTail: Seq[A] = s.tail :+ s.head

    def cyclicDrop(n: Int): Seq[A] = s.drop(n) ++ s.take(n)
  }

  trait Part {
    def captcha(digits: Seq[Int]): Int

    def captcha(digitsStr: String): Int = {
      val digits = digitsStr.toSeq.map(_.asDigit)
      captcha(digits)
    }
  }

  object Part1 extends Part {
    override def captcha(digits: Seq[Int]): Int =
      digits.lazyZip(digits.cyclicTail)
        .map((digit, nextDigit) => if (digit == nextDigit) digit else 0)
        .sum
  }

  object Part2 extends Part {
    override def captcha(digits: Seq[Int]): Int = {
      require(digits.length % 2 == 0)

      digits.lazyZip(digits.cyclicDrop(digits.length / 2))
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
