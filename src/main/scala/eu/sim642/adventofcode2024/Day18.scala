package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day18 {

  def exitSteps(bytes: Seq[Pos], max: Pos = Pos(70, 70), after: Int = 1024): Int = {
    val fallenBytes = bytes.take(after).toSet

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = Pos.zero

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if Pos.zero <= newPos && newPos <= max
          if !fallenBytes(newPos)
        } yield newPos
      }

      override val targetNode: Pos = max
    }

    BFS.search(graphSearch).target.get._2
  }

  def parseByte(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def parseBytes(input: String): Seq[Pos] = input.linesIterator.map(parseByte).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(exitSteps(parseBytes(input)))
  }
}
