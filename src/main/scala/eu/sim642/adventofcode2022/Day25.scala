package eu.sim642.adventofcode2022

import Integral.Implicits.*
import scala.annotation.tailrec

object Day25 {

  type Snafu = String

  def snafuDigit2int(c: Char): Int = c match {
    case '=' => -2
    case '-' => -1
    case '0' => 0
    case '1' => 1
    case '2' => 2
  }

  def snafu2int(snafu: Snafu): Long = {
    snafu.foldLeft(0L)({ (acc, c) =>
      5 * acc + snafuDigit2int(c)
    })
  }

  def int2snafuDigit(d: Int): Char = d match {
    case -2 => '='
    case -1 => '-'
    case 0 => '0'
    case 1 => '1'
    case 2 => '2'
  }

  def int2snafu(int: Long): Snafu = {

    @tailrec
    def int2base5(int: Long, acc: List[Int]): List[Int] = {
      val (q, r) = int /% 5
      val newAcc = r.toInt :: acc
      if (q == 0)
        newAcc
      else
        int2base5(q, newAcc)
    }

    @tailrec
    def base52balBase5(digits: List[Int], carry: Int, acc: List[Int]): List[Int] = digits match {
      case Nil if carry == 0 =>
        acc
      case Nil =>
        carry :: acc
      case digit :: newDigits =>
        val (newDigit, newCarry) = digit + carry match {
          case 0 => (0, 0)
          case 1 => (1, 0)
          case 2 => (2, 0)
          case 3 => (-2, 1)
          case 4 => (-1, 1)
          case 5 => (0, 1)
        }
        val newAcc = newDigit :: acc
        base52balBase5(newDigits, newCarry, newAcc)
    }

    val base5 = int2base5(int, Nil)
    val balBase5 = base52balBase5(base5.reverse, 0, Nil)
    balBase5.map(int2snafuDigit).mkString
  }

  def sumSnafus(snafus: Seq[Snafu]): Snafu =
    int2snafu(snafus.map(snafu2int).sum)


  def parseSnafus(input: String): Seq[Snafu] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumSnafus(parseSnafus(input)))
  }
}
