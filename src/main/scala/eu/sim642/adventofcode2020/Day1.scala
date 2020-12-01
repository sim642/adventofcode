package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day1 {

  def entryProduct2020(entries: Seq[Int], n: Int): Int = {
    entries.combinations(n).filter(_.sum == 2020).head.product
  }

  def parseEntries(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(entryProduct2020(parseEntries(input), 2))
    println(entryProduct2020(parseEntries(input), 3))
  }
}
