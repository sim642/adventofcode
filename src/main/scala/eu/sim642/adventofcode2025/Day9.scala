package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day9 {

  def largestArea(redTiles: Seq[Pos]): Long = {
    (for {
      // faster than combinations(2)
      (p1, i) <- redTiles.iterator.zipWithIndex
      p2 <- redTiles.view.slice(i + 1, redTiles.size).iterator
    } yield Box.bounding(Seq(p1, p2)).size[Long]).max
  }

  def parseRedTile(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def parseRedTiles(input: String): Seq[Pos] = input.linesIterator.map(parseRedTile).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(largestArea(parseRedTiles(input)))
  }
}
