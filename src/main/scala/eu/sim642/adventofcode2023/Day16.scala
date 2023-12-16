package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.GridImplicits._

object Day16 {

  case class Beam(pos: Pos, direction: Pos)

  def countEnergized(grid: Grid[Char], startBeam: Beam = Beam(Pos.zero, Pos(1, 0))): Int = {

    val graphTraversal = new GraphTraversal[Beam] with UnitNeighbors[Beam] {
      override val startNode: Beam = startBeam

      override def unitNeighbors(beam: Beam): IterableOnce[Beam] = {
        val Beam(pos, direction) = beam
        val newDirections =
          grid(pos) match {
            case '.' => Iterator(direction)
            case '/' => Iterator(direction.reflectMinor)
            case '\\' => Iterator(direction.reflectMajor)
            case '|' if direction.x == 0 => Iterator(direction)
            case '-' if direction.y == 0 => Iterator(direction)
            case '|' | '-' => Iterator(direction.left, direction.right)
          }
        for {
          newDirection <- newDirections
          newPos = pos + newDirection
          if grid.containsPos(newPos)
        } yield Beam(newPos, newDirection)
      }
    }

    BFS.traverse(graphTraversal).nodes.map(_.pos).size
  }

  def maxEnergized(grid: Grid[Char]): Int = {
    val top = grid(0).indices.map(x => Beam(Pos(x, 0), Pos(0, 1)))
    val bottom = grid(0).indices.map(x => Beam(Pos(x, grid.size - 1), Pos(0, -1)))
    val left = grid.indices.map(y => Beam(Pos(0, y), Pos(1, 0)))
    val right = grid.indices.map(y => Beam(Pos(grid(0).size - 1, y), Pos(-1, 0)))
    (top ++ bottom ++ left ++ right).map(countEnergized(grid, _)).max
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countEnergized(parseGrid(input)))
    println(maxEnergized(parseGrid(input)))

    // part 2: 7222 - too low (right pos coordinates swapped in maxEnergized)
  }
}
