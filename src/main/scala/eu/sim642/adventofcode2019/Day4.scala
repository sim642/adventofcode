package eu.sim642.adventofcode2019

import scala.Integral.Implicits._
import scala.annotation.tailrec

object Day4 {

  trait Part {
    def isPassword(number: Int): Boolean

    def countPasswords(range: Range.Inclusive): Int = range.count(isPassword)
  }

  // should be faster than number.toString.toList.map(_.asDigit)
  def toDigitList(number: Int): List[Int] = {
    @tailrec
    def helper(number: Int, acc: List[Int]): List[Int] = {
      val (q, r) = number /% 10
      val newAcc = r :: acc
      if (q == 0)
        newAcc
      else
        helper(q, newAcc)
    }

    helper(number, Nil)
  }

  def runLengthsReverse[A](list: List[A]): List[Int] = {
    @tailrec
    def helper(list: List[A], prev: A, run: Int, acc: List[Int]): List[Int] = list match {
      case Nil => run :: acc
      case x :: xs =>
        if (x == prev)
          helper(xs, x, run + 1, acc)
        else
          helper(xs, x, 1, run :: acc)
    }

    list match {
      case Nil => Nil // unnecessary case for this task
      case x :: xs => helper(xs, x, 1, Nil)
    }
  }

  def isSorted(list: List[Int]): Boolean = {
    (list lazyZip list.tail).forall(_ <= _)
  }

  object Part1 extends Part {
    override def isPassword(number: Int): Boolean = {
      val digits = toDigitList(number)
      // seems a bit faster to short-circuit isSorted first
      isSorted(digits) && runLengthsReverse(digits).exists(_ >= 2)
    }
  }

  object Part2 extends Part {
    override def isPassword(number: Int): Boolean = {
      val digits = toDigitList(number)
      // seems a bit faster to short-circuit isSorted first
      isSorted(digits) && runLengthsReverse(digits).contains(2)
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
