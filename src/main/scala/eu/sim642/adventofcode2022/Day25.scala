package eu.sim642.adventofcode2022

import Integral.Implicits.*

object Day25 {

  type Snafu = String

  val snafuDigit2int: PartialFunction[Char, Int] = {
    case '=' => -2
    case '-' => -1
    case '0' => 0
    case '1' => 1
    case '2' => 2
  }

  def snafu2int(snafu: Snafu): Long =
    snafu.foldLeft(0L)(5 * _ + snafuDigit2int(_))

  val int2snafuDigit: PartialFunction[Int, Char] = {
    case -2 => '='
    case -1 => '-'
    case 0 => '0'
    case 1 => '1'
    case 2 => '2'
  }

  def int2snafu(int: Long): Snafu = {
    Seq.unfold(int)({ int =>
      if (int == 0)
        None
      else {
        val (q, r) = int /% 5
        if (r <= 2)
          Some((r, q))
        else
          Some((r - 5, q + 1)) // subtract from carried instead
      }
    })
      .view
      .reverse
      .map(_.toInt)
      .map(int2snafuDigit)
      .mkString
  }

  def sumSnafus(snafus: Seq[Snafu]): Snafu =
    int2snafu(snafus.map(snafu2int).sum)


  def parseSnafus(input: String): Seq[Snafu] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumSnafus(parseSnafus(input)))
  }
}
