package eu.sim642.adventofcode2018

import scala.language.implicitConversions
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

object Day2 {

  def letterCounts(id: String): Map[Char, Int] = id.toSeq.groupCount(identity)

  given Conversion[Boolean, Int] = if (_) 1 else 0

  def checksum(ids: Seq[String]): Int = {
    val (cnt2, cnt3) = ids.foldLeft((0, 0)) { case ((cnt2, cnt3), id) =>
      val idCounts = letterCounts(id).values.toSet
      (cnt2 + idCounts.contains(2), cnt3 + idCounts.contains(3))
    }
    cnt2 * cnt3
  }

  def commonCorrectIds(ids: Seq[String]): String = {
    (for {
      Seq(id1, id2) <- ids.combinations(2)
      common = id1.zip(id2).filter({ case (c1, c2) => c1 == c2 }).map(_._1)
      if common.length == id1.length - 1
    } yield common.mkString("")).head
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim
  lazy val inputLines: Seq[String] = input.linesIterator.toSeq

  def main(args: Array[String]): Unit = {
    println(checksum(inputLines))
    println(commonCorrectIds(inputLines))
  }
}
