package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day23 {

  case class HikePos(pos: Pos, pathSet: Set[Pos])

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

      val keyNeighbors: Map[Pos, Map[Pos, Int]] = {
        keyPoss.view.map({ fromPos =>
          val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
            override val startNode: Pos = fromPos

            override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
              if (pos != fromPos && keyPoss(pos))
                Iterator.empty
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

          val distances = BFS.traverse(graphTraversal).distances.toMap
          fromPos -> distances.filter(p => p._1 != fromPos && keyPoss(p._1))
        }).toMap
      }

      val graphTraversal = new GraphTraversal[HikePos] {
        override val startNode: HikePos = HikePos(startPos, Set(startPos))

        override def neighbors(hikePos: HikePos): IterableOnce[(HikePos, Int)] = {
          if (hikePos.pos != targetPos) {
            val HikePos(pos, pathSet) = hikePos
            for {
              (newPos, dist) <- keyNeighbors(pos)
              if !pathSet.contains(newPos)
            } yield HikePos(newPos, pathSet + newPos) -> dist
          }
          else
            Iterator.empty
        }
      }

      @tailrec
      def helper(todo: Set[HikePos], visited: Map[HikePos, Int]): Map[HikePos, Int] = {
        if (todo.isEmpty)
          visited.filter(_._1.pathSet.contains(targetPos))
        else {
          val hikePos = todo.head
          val newTodo = todo - hikePos
          val visited0 = visited
          val oldDist = visited0(hikePos)
          val (newTodo2, newVisited) = graphTraversal.neighbors(hikePos).iterator.foldLeft((newTodo, visited))({ case ((todo, visited), (newHikePos2, dist)) =>
            val newDist = oldDist + dist
            if (visited0.contains(newHikePos2)) {
              if (visited0(newHikePos2) < newDist) {
                (todo + newHikePos2, visited + (newHikePos2 -> newDist))
              }
              else {
                (todo, visited)
              }
            }
            else {
              (todo + newHikePos2, visited + (newHikePos2 -> newDist))
            }
          })
          helper(newTodo2, newVisited)
        }
      }

      val distances = helper(Set(graphTraversal.startNode), Map(graphTraversal.startNode -> 0))
      distances.values.max
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
