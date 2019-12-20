package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.box.Box
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

  def stepsRecursive(grid: Grid[Tile]): Int = {

    val startPortal = Portal('A', 'A')
    val targetPortal = Portal('Z', 'Z')

    val innerBox = Box(Pos(3, 3), Pos(grid.map(_.size).max - 1 - 3, grid.size - 1 - 3))

    case class Node(pos: Pos, level: Int)

    val graphSearch = new GraphSearch[Node] with UnitNeighbors[Node] with TargetNode[Node] {
      override val startNode: Node = Node(grid.posOf(startPortal), 0)

      override def unitNeighbors(node: Node): IterableOnce[Node] = {
        val Node(pos, level) = node

        val moveNeighbors = for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if grid(newPos) != Wall
        } yield Node(newPos, level)

        val portalNeighbors = grid(pos) match {
          case portal@Portal(_, _) if portal != startPortal && portal != targetPortal =>
            //val portalPos = grid.posOf(Portal(to, from))
            // TODO: Grid posWhere
            val portalPos =
              (for {
                (row, y) <- grid.view.zipWithIndex
                (cell, x) <- row.view.zipWithIndex
                portalPos = Pos(x, y)
                if portalPos != pos
                if cell == portal
              } yield Pos(x, y))
            println(portalPos.toSeq)
            for (pp <- portalPos) {
              println(pp, grid(pp), portal)
            }
            assert(portalPos.size == 1)
            val newLevel = if (innerBox.contains(pos)) level + 1 else level - 1
            if (newLevel < 0)
              Iterator.empty
            else
              Iterator(Node(portalPos.head, newLevel))
          case _ => Iterator.empty
        }

        moveNeighbors ++ portalNeighbors
      }

      override val targetNode: Node = Node(grid.posOf(targetPortal), 0)
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
    println(stepsRecursive(parseGrid(input)))

    // 700 - too high
  }
}
