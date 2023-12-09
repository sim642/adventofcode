package eu.sim642.adventofcode2015

import scala.annotation.tailrec

object Day8 {

  def unescapedLength(s: String): Int = {

    @tailrec
    def helper(l: List[Char], length: Int): Int = l match {
      case Nil => length
      case '\\' :: ('\\' | '"') :: tl => helper(tl, length + 1)
      case '\\' :: 'x' :: _ :: _ :: tl => helper(tl, length + 1)
      case '\\' :: _ :: _ => throw new IllegalArgumentException("illegal escape char")
      case _ :: tl => helper(tl, length + 1)
    }

    val inQuotes = s.substring(1, s.length - 1)
    helper(inQuotes.toList, 0)
  }

  def unescapeLengthDiff(input: String): Int = {
    input.linesIterator.map(s => s.length - unescapedLength(s)).sum
  }

  def escapedLength(s: String): Int = {

    @tailrec
    def helper(l: List[Char], length: Int): Int = l match {
      case Nil => length
      case ('\\' | '"') :: tl => helper(tl, length + 2)
      case _ :: tl => helper(tl, length + 1)
    }

    helper(s.toList, 0) + 2
  }

  def escapeLengthDiff(input: String): Int = {
    input.linesIterator.map(s => escapedLength(s) - s.length).sum
  }


  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(unescapeLengthDiff(input))
    println(escapeLengthDiff(input))
  }
}
