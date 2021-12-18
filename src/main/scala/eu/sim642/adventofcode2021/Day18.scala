package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day18 extends RegexParsers {

  sealed trait Number {
    def +(that: Number): Number = reduce(Pair(this, that))
    def addLeft(addValue: Int): Number
    def addRight(addValue: Int): Number
    def magnitude: Int
  }
  case class Regular(value: Int) extends Number {
    override def addLeft(addValue: Int): Number = Regular(value + addValue)
    override def addRight(addValue: Int): Number = Regular(value + addValue)
    override def magnitude: Int = value
  }
  case class Pair(left: Number, right: Number) extends Number {
    override def addLeft(addValue: Int): Number = Pair(left.addLeft(addValue), right)
    override def addRight(addValue: Int): Number = Pair(left, right.addRight(addValue))
    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  def explode(number: Number, depth: Int): Option[(Option[Int], Number, Option[Int])] = number match {
    case Regular(_) =>
      None
    case Pair(Regular(left), Regular(right)) if depth >= 4 =>
      Some((Some(left), Regular(0), Some(right)))
    case Pair(left, right) =>
      explode(left, depth + 1).map((leftAdd, left, rightAdd) =>
        (leftAdd, Pair(left, rightAdd.map(right.addLeft).getOrElse(right)), None)
      ) orElse explode(right, depth + 1).map((leftAdd, right, rightAdd) =>
        (None, Pair(leftAdd.map(left.addRight).getOrElse(left), right), rightAdd)
      )
  }

  def explode(number: Number): Option[Number] = explode(number, 0).map(_._2)

  def split(number: Number): Option[Number] = number match {
    case Regular(value) if value >= 10 =>
      val halfValue = value.toFloat / 2
      Some(Pair(Regular(halfValue.floor.toInt), Regular(halfValue.ceil.toInt)))
    case Regular(_) =>
      None
    case Pair(left, right) =>
      split(left).map(Pair(_, right)) orElse split(right).map(Pair(left, _))
  }

  @tailrec
  def reduce(number: Number): Number = {
    explode(number) match {
      case Some(number) => reduce(number)
      case None =>
        split(number) match {
          case Some(number) => reduce(number)
          case None => number
        }
    }
  }

  def addNumbers(numbers: Seq[Number]): Number = numbers.reduce(_ + _)

  def addNumbersMagnitude(numbers: Seq[Number]): Int = addNumbers(numbers).magnitude

  def largestTwoMagnitude(numbers: Seq[Number]): Int = {
    (for {
      left <- numbers.iterator
      right <- numbers.iterator
    } yield (left + right).magnitude).max
  }


  def parseNumber(s: String): Number = {

    def number: Parser[Number] = (
      "\\d+".r ^^ (_.toInt) ^^ Regular.apply
    | "[" ~> number ~ "," ~ number <~ "]" ^^ { case left ~ _ ~ right => Pair(left, right) }
    )

    parseAll(number, s).get
  }

  def parseNumbers(input: String): Seq[Number] = input.linesIterator.map(parseNumber).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(addNumbersMagnitude(parseNumbers(input)))
    println(largestTwoMagnitude(parseNumbers(input)))
  }
}
