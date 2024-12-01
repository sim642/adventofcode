package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IterableImplicits._

object Day1 {

  def totalListDistance(lists: (Seq[Int], Seq[Int])): Int = {
    val (left, right) = lists
    (left.sorted lazyZip right.sorted)
      .map((i, j) => (i - j).abs)
      .sum
  }

  def similarityScore(lists: (Seq[Int], Seq[Int])): Int = {
    val (left, right) = lists
    val rightCount = right.groupCount(identity).withDefaultValue(0)
    left
      .map(i => i * rightCount(i))
      .sum
  }

  def parsePair(s: String): (Int, Int) = s match {
    case s"$i   $j" => (i.toInt, j.toInt)
  }

  def parseLists(input: String): (Seq[Int], Seq[Int]) = input.linesIterator.map(parsePair).toSeq.unzip

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalListDistance(parseLists(input)))
    println(similarityScore(parseLists(input)))
  }
}
