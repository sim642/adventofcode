package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day10 {

  def lookAndSay(s: String): String = {
    val sb = new StringBuilder
    var i = 0
    while (i < s.length) {
      val c = s(i)
      val segmentLength = 1 + s.segmentLength(_ == c, i + 1)
      sb.append(segmentLength).append(c)
      i += segmentLength
    }
    sb.toString
  }

  def lookAndSayIterator(s: String): Iterator[String] = Iterator.iterate(s)(lookAndSay)

  def lookAndSayLength(s: String, times: Int): Int = lookAndSayIterator(s)(times).length


  //lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim
  val input = "1321131112"

  def main(args: Array[String]): Unit = {
    println(lookAndSayLength(input, 40))
    println(lookAndSayLength(input, 50))
  }
}
