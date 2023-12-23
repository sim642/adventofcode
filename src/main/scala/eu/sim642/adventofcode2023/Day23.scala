package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day23 {

  private val slopeOffsets = Map(
    '^' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
  )

  trait Part {
    protected val slopes: Boolean

    def longestHike(grid: Grid[Char]): Int = {
      val startPos = Pos(grid.head.indexOf('.'), 0)
      val targetPos = Pos(grid.last.indexOf('.'), grid.size - 1)

      val branchPoss = (for {
        pos <- Box(Pos(1, 1), Pos(grid(0).size - 2, grid.size - 2)).iterator
        if grid(pos) != '#'
        if Pos.axisOffsets.count(offset => grid(pos + offset) != '#') > 2
      } yield pos).toSet

      val keyPoss = branchPoss + startPos + targetPos

      // based on 2019 day 18
      val keyAdjacents: Map[Pos, Map[Pos, Int]] = {
        (branchPoss + startPos).view.map({ fromPos =>
          val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
            override val startNode: Pos = fromPos

            override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
              if (pos != fromPos && keyPoss(pos))
                Iterator.empty // don't move beyond bounding keys
              else {
                for {
                  offset <- if (!slopes || grid(pos) == '.') Pos.axisOffsets else Seq(slopeOffsets(grid(pos)))
                  newPos = pos + offset
                  if grid.containsPos(newPos)
                  if grid(newPos) != '#'
                } yield newPos
              }
            }
          }

          val distances = BFS.traverse(graphTraversal).distances
          val keyDistances = distances.filter({ (toPos, _) =>
            toPos != fromPos && keyPoss(toPos)
          }).toMap

          fromPos -> keyDistances
        }).toMap
      }

      // based on https://github.com/glguy/advent/blob/9c27a67f756c44b7f0711539ee5e892dca252791/solutions/src/2023/23.hs
      def helper(pos: Pos, keyAdjacents: Map[Pos, Map[Pos, Int]], dist: Int): Int = {
        if (pos == targetPos)
          dist
        else {
          keyAdjacents.get(pos) match {
            case Some(adjacents) =>
              val newKeyAdjacents = keyAdjacents - pos
              (for {
                (toPos, cost) <- adjacents
              } yield helper(toPos, newKeyAdjacents, dist + cost)).max
            case None => 0
          }
        }
      }

      helper(startPos, keyAdjacents, 0)
    }
  }

  object Part1 extends Part {
    override protected val slopes: Boolean = true
  }

  object Part2 extends Part {
    override protected val slopes: Boolean = false
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.longestHike(parseGrid(input)))
    println(Part2.longestHike(parseGrid(input)))
  }
}
