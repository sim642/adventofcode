package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.IteratorImplicits._
import scala.annotation.tailrec

object Day11 {

  def increment(s: String): String = {
    @tailrec
    def helper(s: String, i: Int): String = {
      if (i < 0)
        s
      else {
        s(i) match {
          case 'z' =>
            val newS = s.patch(i, "a", 1)
            helper(newS, i - 1)
          case c =>
            s.patch(i, (c + 1).toChar.toString, 1)
        }
      }
    }

    helper(s, s.length - 1)
  }

  def iterateIncrements(s: String): Iterator[String] = {
    Iterator.iterate(s)(increment)
  }

  def req1(s: String): Boolean = {
    @tailrec
    def helper(prev: Char, i: Int, count: Int): Boolean = {
      if (count == 3)
        true
      else if (i == s.length)
        false
      else {
        val cur = s(i)
        val newCount = if (prev + 1 == cur) count + 1 else 1
        helper(cur, i + 1, newCount)
      }
    }

    helper(s.head, 1, 1)
  }

  def req2(s: String): Boolean = {
    // faster than [^iol]* regex
    !s.contains('i') && !s.contains('o') && !s.contains('l')
  }

  private val req3Regex = """(.)\1.*((?!\1).)\2""".r

  def req3(s: String): Boolean = {
    req3Regex.findFirstIn(s).isDefined
  }

  def findPassword(s: String, i: Int = 0): String = {
    iterateIncrements(s).filter(s => req1(s) && req2(s) && req3(s))(i)
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim
  val input = "hxbxwxba"

  def main(args: Array[String]): Unit = {
    println(findPassword(input))
    println(findPassword(input, 1))
  }
}
