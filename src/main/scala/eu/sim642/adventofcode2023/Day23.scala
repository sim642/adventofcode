package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box

object Day23 {

  case class HikePos(pos: Pos, path: List[Pos], pathSet: Set[Pos])

  private val slopeOffsets = Map(
    '^' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
  )

  def longestHike(grid: Grid[Char], slopes: Boolean = true): Int = {

    val startPos = Pos(1, 0) // TODO: don't hardcode
    val targetPos = Pos(grid(0).size - 2, grid.size - 1)

    val branchPoss = (for {
      pos <- Box(Pos(1, 1), Pos(grid(0).size - 2, grid.size - 2)).iterator
      if grid(pos) != '#'
      if Pos.axisOffsets.count(offset => grid(pos + offset) != '#') > 2
    } yield pos).toSet

    val keyPoss = branchPoss + startPos + targetPos
    println(keyPoss)

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
      override val startNode: HikePos = HikePos(startPos, List(startPos), Set(startPos))

      override def neighbors(hikePos: HikePos): IterableOnce[(HikePos, Int)] = {
        val HikePos(pos, path, pathSet) = hikePos
        for {
          (newPos, dist) <- keyNeighbors(pos)
          //() = println((pos, newPos, dist))
          //() = assert(slopes || keyNeighbors(newPos)(pos) == dist)
          if !path.contains(newPos)
        } yield HikePos(newPos, newPos :: path, pathSet + newPos) -> dist
      }
    }


    val asd = Dijkstra.traverse(graphTraversal).distances
      .filter(_._1.pos == targetPos)

    asd.values.toSet.foreach(println)
    println(asd.maxBy(_._2)._1.path)
    asd
      .values
      .max
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //println(longestHike(parseGrid(input)))
    println(longestHike(parseGrid(input), slopes = false))
  }
}
