package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph.{AStar, BFS, GraphSearch, Heuristic, TargetNode, UnitNeighbors}

object Day13 {

  def isOpen(pos: Pos, favorite: Int): Boolean = {
    val Pos(x, y) = pos
    val value = (x*x + 3*x + 2*x*y + y + y*y) + favorite
    Integer.bitCount(value) % 2 == 0
  }

  def getNeighbors(pos: Pos, favorite: Int): Seq[Pos] = {
    for {
      offset <- Pos.axisOffsets
      newPos = pos + offset
      if newPos.x >= 0 && newPos.y >= 0
      if isOpen(newPos, favorite)
    } yield newPos
  }

  def fewestSteps(favorite: Int, targetPos: Pos = Pos(31, 39)): Int = {
    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] with Heuristic[Pos] {
      override val startNode: Pos = Pos(1, 1)

      override def unitNeighbors(pos: Pos): TraversableOnce[Pos] = getNeighbors(pos, favorite)

      override val targetNode: Pos = targetPos

      override def heuristic(pos: Pos): Int = pos manhattanDistance targetNode
    }

    AStar.search(graphSearch).target.get._2
  }

  def reachableLocations(favorite: Int, maxDist: Int = 50): Int = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = Pos(1, 1)

      override def unitNeighbors(pos: Pos): TraversableOnce[Pos] = getNeighbors(pos, favorite)

      override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == maxDist
    }

    BFS.search(graphSearch).nodes.size
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim
  val input = 1364

  def main(args: Array[String]): Unit = {
    println(fewestSteps(input))
    println(reachableLocations(input))
  }
}
