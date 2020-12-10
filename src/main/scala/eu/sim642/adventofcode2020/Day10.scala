package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day10 {

  def differencesProductAll(jolts: Seq[Int]): Int = {
    val initialJolt = 0
    val builtinJolt = jolts.max + 3
    val allJolts = initialJolt +: builtinJolt +: jolts
    val diffs = allJolts.sorted.iterator.zipWithTail.map({ case (a, b) => b - a }).toSeq
    diffs.count(_ == 1) * diffs.count(_ == 3)
  }


  def parseJolts(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(differencesProductAll(parseJolts(input)))
  }
}
