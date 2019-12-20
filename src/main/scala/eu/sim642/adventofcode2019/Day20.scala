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
  case class Portal(label: String) extends Tile

  private val startPortal = Portal("AA")
  private val targetPortal = Portal("ZZ")

  object NormalPortal {
    def unapply(tile: Tile): Option[Portal] = tile match {
      case portal@Portal(_) if portal != startPortal && portal != targetPortal =>
        Some(portal)
      case _ => None
    }
  }

  def findOtherPortalPos(grid: Grid[Tile], portalPos: Pos): Pos = {
    val portal = grid(portalPos)
    // TODO: Grid posWhere? would need pos argument for this though
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      pos = Pos(x, y)
      if pos != portalPos
      if cell == portal
    } yield Pos(x, y)).head
  }

  def steps(grid: Grid[Tile]): Int = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = grid.posOf(startPortal)

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        val moveNeighbors = for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if grid(newPos) != Wall
        } yield newPos

        val portalNeighbors = grid(pos) match {
          case NormalPortal(_) =>
            val otherPos = findOtherPortalPos(grid, pos)
            Iterator(otherPos)
          case _ => Iterator.empty
        }

        moveNeighbors ++ portalNeighbors
      }

      override val targetNode: Pos = grid.posOf(targetPortal)
    }

    BFS.search(graphSearch).target.get._2
  }

  def stepsRecursive(grid: Grid[Tile]): Int = {
    // TODO: reduce duplication

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
          case NormalPortal(_) =>
            val otherPos = findOtherPortalPos(grid, pos)
            if (innerBox.contains(pos))
              Iterator(Node(otherPos, level + 1))
            else if (level > 0)
              Iterator(Node(otherPos, level - 1))
            else
              Iterator.empty
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

      label = if (pos1 <= pos2) s"$cell1$cell2" else s"$cell2$cell1"
    } yield pos -> label).toMap

    val grid = portals.foldLeft(wallGrid)({ case (grid, (pos, label)) =>
      grid.updatedGrid(pos, Portal(label))
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
