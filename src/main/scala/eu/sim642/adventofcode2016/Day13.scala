package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcodelib.{GraphSearch, Heuristic, UnitNeighbors}

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
    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with Heuristic[Pos] {
      override def startNodes: TraversableOnce[Pos] = Seq(Pos(1, 1))

      override def isTargetNode(pos: Pos): Boolean = pos == targetPos

      override def unitNeighbors(pos: Pos): TraversableOnce[Pos] = getNeighbors(pos, favorite)

      override def heuristic(pos: Pos): Int = pos manhattanDistance targetPos
    }

    GraphSearch.aStar(graphSearch).target.get._2
  }

  // bfs
  def reachableLocations(favorite: Int, maxDist: Int = 50): Int = {

    def helper(visited: Set[Pos], toVisit: Set[Pos], dist: Int): Set[Pos] = {
      val neighbors = toVisit.flatMap(getNeighbors(_, favorite))
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors -- visited
      if (newToVisit.isEmpty || dist == maxDist)
        newVisited
      else
        helper(newVisited, newToVisit, dist + 1)
    }

    helper(Set.empty, Set(Pos(1, 1)), 0).size
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim
  val input = 1364

  def main(args: Array[String]): Unit = {
    println(fewestSteps(input))
    println(reachableLocations(input))
  }
}
