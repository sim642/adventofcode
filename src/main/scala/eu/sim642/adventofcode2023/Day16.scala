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
        val ret =
          grid(pos) match {
            case '.' => Seq(beam.copy(pos = pos + direction))
            case '/' =>
              val newDirection = direction.reflectMinor
              Seq(beam.copy(pos = pos + newDirection, direction = newDirection))
            case '\\' =>
              val newDirection = direction.reflectMajor
              Seq(beam.copy(pos = pos + newDirection, direction = newDirection))
            case '|' if direction.x == 0 => Seq(beam.copy(pos = pos + direction))
            case '-' if direction.y == 0 => Seq(beam.copy(pos = pos + direction))
            case '|' | '-' =>
              for {
                newDirection <- Seq(direction.left, direction.right)
              } yield beam.copy(pos = pos + newDirection, direction = newDirection)
          }
        ret.filter(b => grid.containsPos(b.pos))
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
