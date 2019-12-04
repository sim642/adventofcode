package eu.sim642.adventofcode2019

object Day4 {

  def isPassword(number: Int): Boolean = {
    val digits = number.toString.toList.map(_.asDigit)
    val deltas = (digits.tail lazyZip digits).map(_ - _)
    deltas.contains(0) && // two adjacent are the same
      deltas.forall(_ >= 0) // digits never decrease
  }

  def countPasswords(range: Range.Inclusive): Int = range.count(isPassword)

  def parseRange(input: String): Range.Inclusive = {
    val Seq(lo, hi) = input.split('-').toSeq.map(_.toInt)
    lo to hi
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  val input: String = "278384-824795"

  def main(args: Array[String]): Unit = {
    println(countPasswords(parseRange(input)))
  }
}
