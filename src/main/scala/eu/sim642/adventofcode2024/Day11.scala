package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IterableImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day11 {

  def blinkStone(stone: Long): Seq[Long] = {
    if (stone == 0)
      Seq(1)
    else {
      val stoneStr = stone.toString
      if (stoneStr.length % 2 == 0) {
        val halfLength = stoneStr.length / 2
        val (newStone1, newStone2) = stoneStr.splitAt(halfLength)
        Seq(newStone1.toLong, newStone2.toLong)
      }
      else
        Seq(stone * 2024)
    }
  }

  def countBlinkedStones(stones: Seq[Long], blinks: Int): Long = {
    val initialFreqs = stones.groupCount(identity).view.mapValues(_.toLong).toMap

    def blinkFreqs(freqs: Map[Long, Long]): Map[Long, Long] = {
      (for {
        (stone, freq) <- freqs.iterator
        newStone <- blinkStone(stone)
      } yield newStone -> freq).groupMapReduce(_._1)(_._2)(_ + _)
    }

    val finalFreqs = Iterator.iterate(initialFreqs)(blinkFreqs)(blinks)
    finalFreqs.values.sum
  }

  def parseStones(input: String): Seq[Long] = input.split(" ").map(_.toLong).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlinkedStones(parseStones(input), 25))
  }
}
