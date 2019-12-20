package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph._
import eu.sim642.adventofcodelib.pos.Pos

object Day20 {

  sealed trait Tile
  case object Wall extends Tile
  case object Open extends Tile
  case class Portal(from: Char, to: Char) extends Tile

  def steps(grid: Grid[Tile]): Int = {

    val startPortal = Portal('A', 'A')
    val targetPortal = Portal('Z', 'Z')

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = grid.posOf(startPortal)

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        val moveNeighbors = for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if grid(newPos) != Wall
        } yield newPos

        val portalNeighbors = grid(pos) match {
          case portal@Portal(from, to) if portal != startPortal && portal != targetPortal =>
            //val portalPos = grid.posOf(Portal(to, from))
            // TODO: Grid posWhere
            val portalPos =
              (for {
                (row, y) <- grid.view.zipWithIndex
                (cell, x) <- row.view.zipWithIndex
                portalPos = Pos(x, y)
                if portalPos != pos
                if cell == portal// || cell == Portal(to, from)
              } yield Pos(x, y))
            println(portalPos.toSeq)
            assert(portalPos.size == 1)
            Iterator(portalPos.head)
          case _ => Iterator.empty
        }

        moveNeighbors ++ portalNeighbors
      }

      override val targetNode: Pos = grid.posOf(targetPortal)
    }

    BFS.search(graphSearch).target.get._2
  }

  def parseGrid(input: String): Grid[Tile] = {
    val charGrid = input.linesIterator.map(_.toVector).toVector

    val wallGrid = charGrid.mapGrid({
      case '.' => Open
      case _ => Wall
    })

    // TODO: refactor Grid iterations
    val portals = (for {
      (row, y) <- wallGrid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == Open
      pos = Pos(x, y)
      offset <- Pos.axisOffsets
      pos1 = pos + offset
      cell1 = charGrid(pos1)
      if cell1.isLetter
      pos2 = pos + 2 *: offset
      cell2 = charGrid(pos2)
      if cell2.isLetter
    } yield pos -> (if (pos1 <= pos2) cell1 -> cell2 else cell2 -> cell1)).toMap

    val grid = portals.foldLeft(wallGrid)({ case (grid, (pos, (from, to))) =>
      grid.updatedGrid(pos, Portal(from, to))
    })

    grid
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(steps(parseGrid(input)))

    // 700 - too high
  }
}
