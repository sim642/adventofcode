package eu.sim642.adventofcode2018

import scala.language.implicitConversions

object Day2 {
  def letterCounts(id: String): Map[Char, Int] = id.groupBy(c => c).mapValues(_.length)

  implicit def bool2Int(b: Boolean): Int = if (b) 1 else 0

  def checksum(ids: Seq[String]): Int = {
    val (cnt2, cnt3) = ids.foldLeft((0, 0)) { case ((cnt2, cnt3), id) =>
      val idCounts = letterCounts(id).values.toSet
      (cnt2 + idCounts.contains(2), cnt3 + idCounts.contains(3))
    }
    cnt2 * cnt3
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim
  lazy val inputLines: Seq[String] = input.lines.toSeq

  def main(args: Array[String]): Unit = {
    println(checksum(inputLines))
  }
}
