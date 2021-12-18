package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day18 extends RegexParsers {

  sealed trait Number {
    def +(that: Number): Number = reduce(Pair(this, that))
    def magnitude: Int
  }
  case class Regular(value: Int) extends Number {
    override def magnitude: Int = value
  }
  case class Pair(left: Number, right: Number) extends Number {
    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  def addLeft(number: Number, addValue: Int): Number = number match {
    case Regular(value) => Regular(value + addValue)
    case Pair(left, right) => Pair(addLeft(left, addValue), right)
  }

  def addRight(number: Number, addValue: Int): Number = number match {
    case Regular(value) => Regular(value + addValue)
    case Pair(left, right) => Pair(left, addRight(right, addValue))
  }

  def explode(number: Number, depth: Int): Either[Number, (Option[Int], Number, Option[Int])] = number match {
    case Regular(_) => Left(number)
    case Pair(Regular(left), Regular(right)) if depth >= 4 =>
      Right((Some(left), Regular(0), Some(right)))
    case Pair(left, right) =>
      explode(left, depth + 1) match {
        case Right((leftAdd, left, Some(rightAdd))) =>
          Right((leftAdd, Pair(left, addLeft(right, rightAdd)), None))
        case Right((leftAdd, left, None)) =>
          Right((leftAdd, Pair(left, right), None))
        case Left(left) =>
          explode(right, depth + 1) match {
            case Right((Some(leftAdd), right, rightAdd)) =>
              Right((None, Pair(addRight(left, leftAdd), right), rightAdd))
            case Right((None, right, rightAdd)) =>
              Right((None, Pair(left, right), rightAdd))
            case Left(right) =>
              Left(Pair(left, right))
          }
      }
  }

  def explode(number: Number): Either[Number, Number] = explode(number, 0) match {
    case Left(number) => Left(number)
    case Right((_, number, _)) => Right(number)
  }

  def split(number: Number): Either[Number, Number] = number match {
    case Regular(value) if value >= 10 => Right(Pair(Regular((value.toFloat / 2).floor.toInt), Regular((value.toFloat / 2).ceil.toInt)))
    case Regular(value) => Left(number)
    case Pair(left, right) =>
      split(left) match {
        case Right(left) =>
          Right(Pair(left, right))
        case Left(left) =>
          split(right) match {
            case Right(right) =>
              Right(Pair(left, right))
            case Left(right) =>
              Left(Pair(left, right))
          }
      }
  }

  @tailrec
  def reduce(number: Number): Number = {
    explode(number) match {
      case Right(number) => reduce(number)
      case Left(number) =>
        split(number) match {
          case Right(number) => reduce(number)
          case Left(number) => number
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
