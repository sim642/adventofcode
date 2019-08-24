package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents}
import eu.sim642.adventofcodelib.pos.Pos4

object Day25 {

  def bfsGroups(poss: Set[Pos4]): Set[Set[Pos4]] = {

    val graphComponents = new GraphComponents[Pos4] {
      override def nodes: TraversableOnce[Pos4] = poss

      override def unitNeighbors(pos: Pos4): TraversableOnce[Pos4] = poss.filter(_.manhattanDistance(pos) <= 3)
    }

    BFS.components(graphComponents)
  }

  def countConstellations(points: Seq[Pos4]): Int = bfsGroups(points.toSet).size

  def parsePoint(s: String): Pos4 = {
    val Seq(x, y, z, w) = s.split(",").toSeq.map(_.toInt)
    Pos4(x, y, z, w)
  }

  def parseInput(input: String): Seq[Pos4] = input.lines.map(parsePoint).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countConstellations(parseInput(input)))
  }
}
