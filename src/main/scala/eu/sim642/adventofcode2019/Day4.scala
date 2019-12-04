package eu.sim642.adventofcode2019

import scala.collection.immutable.ArraySeq

object Day4 {

  trait Part {
    def isPassword(number: Int): Boolean

    def countPasswords(range: Range.Inclusive): Int = range.count(isPassword)
  }

  object Part1 extends Part {
    override def isPassword(number: Int): Boolean = {
      val digits = number.toString.toList.map(_.asDigit)
      val deltas = (digits.tail lazyZip digits).map(_ - _)
      deltas.contains(0) && // two adjacent are the same
        deltas.forall(_ >= 0) // digits never decrease
    }
  }

  object Part2 extends Part {
    override def isPassword(number: Int): Boolean = {
      val digits = number.toString.to(ArraySeq).map(_.asDigit)
      val deltas = (digits.tail lazyZip digits).map(_ - _)
      deltas.indices.exists({ i =>
        deltas(i) == 0 && // two adjacent are the same
          (i == 0 || deltas(i - 1) != 0) && // not same before
          (i == deltas.size - 1 || deltas(i + 1) != 0) // not same after
      }) && // TODO: make less ugly
        deltas.forall(_ >= 0) // digits never decrease
    }
  }

  def parseRange(input: String): Range.Inclusive = {
    val Seq(lo, hi) = input.split('-').toSeq.map(_.toInt)
    lo to hi
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  val input: String = "278384-824795"

  def main(args: Array[String]): Unit = {
    println(Part1.countPasswords(parseRange(input)))
    println(Part2.countPasswords(parseRange(input)))

    // 673 - too high
    // 544 - too low
  }
}
