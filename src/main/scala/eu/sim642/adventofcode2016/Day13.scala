package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day3.Pos

import scala.collection.mutable
import scala.util.control.Breaks._

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

  // copied from 2018 Day 22, A*
  def fewestSteps(favorite: Int, targetPos: Pos = Pos(31, 39)): Int = {
    val visitedDistance: mutable.Map[Pos, Int] = mutable.Map.empty
    val toVisit: mutable.PriorityQueue[(Int, Int, Pos)] = mutable.PriorityQueue.empty(Ordering.by(-_._1))

    val startPos = Pos(1, 1)

    def heuristic(pos: Pos): Int = pos manhattanDistance targetPos

    def enqueueHeuristically(pos: Pos, dist: Int): Unit = {
      toVisit.enqueue((dist + heuristic(pos), dist, pos))
    }

    enqueueHeuristically(startPos, 0)

    breakable {
      while (toVisit.nonEmpty) {
        val (_, dist, pos) = toVisit.dequeue()
        if (!visitedDistance.contains(pos)) {
          visitedDistance(pos) = dist

          if (pos == targetPos)
            break()

          def goNeighbor(newPos: Pos): Unit = {
            if (!visitedDistance.contains(newPos)) { // avoids some unnecessary queue duplication but not all
              val newDist = dist + 1
              enqueueHeuristically(newPos, newDist)
            }
          }

          getNeighbors(pos, favorite).foreach(goNeighbor)
        }
      }
    }

    visitedDistance(targetPos)
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
