package eu.sim642.adventofcode2015

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
    s.toSeq.sliding(3).exists(s => (s(0) + 1 == s(1)) && (s(1) + 1 == s(2)))
  }

  def req2(s: String): Boolean = {
    !s.contains('i') && !s.contains('o') && !s.contains('l')
  }

  private val req3Regex = """(.)\1.*((?!\1).)\2""".r

  def req3(s: String): Boolean = {
    req3Regex.findFirstIn(s).isDefined
  }

  def findPassword(s: String): String = {
    iterateIncrements(s).find(s => req1(s) && req2(s) && req3(s)).get
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim
  val input = "hxbxwxba"

  def main(args: Array[String]): Unit = {
    println(findPassword(input))
  }
}
