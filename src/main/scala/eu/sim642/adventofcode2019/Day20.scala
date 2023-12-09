package eu.sim642.adventofcode2019

import Day20.Tile._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph._
import eu.sim642.adventofcodelib.pos.Pos

object Day20 {

  enum Tile {
    case Wall
    case Open
    case Portal(label: String)
  }

  private val startPortal = Portal("AA")
  private val targetPortal = Portal("ZZ")

  object NormalPortal {
    def unapply(tile: Tile): Option[Portal] = tile match {
      case portal@Portal(_) if portal != startPortal && portal != targetPortal =>
        Some(portal)
      case _ => None
    }
  }

  def steps(input: Input): Int = {
    val Input(grid, portals) = input

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = grid.posOf(startPortal)

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        val moveNeighbors = for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if grid(newPos) != Wall
        } yield newPos

        val portalNeighbors = grid(pos) match {
          case NormalPortal(portal) =>
            val otherPos = (portals(portal) - pos).head
            Iterator(otherPos)
          case _ => Iterator.empty
        }

        moveNeighbors ++ portalNeighbors
      }

      override val targetNode: Pos = grid.posOf(targetPortal)
    }

    BFS.search(graphSearch).target.get._2
  }

  def stepsRecursive(input: Input): Int = {
    // TODO: reduce duplication
    val Input(grid, portals) = input

    val innerBox = Box(Pos(3, 3), Pos(grid.map(_.size).max - 1 - 3, grid.size - 1 - 3))

    // TODO: generalize "landmark BFS" - BFS + Dijkstra
    val portalPoss = portals.values.flatten.toSet
    val portalPosNeighbors: Map[Pos, Map[Pos, Int]] = portalPoss.view.map({ fromPortalPos =>

        val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
          override val startNode: Pos = fromPortalPos

          override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
            for {
              offset <- Pos.axisOffsets.iterator
              newPos = pos + offset
              if grid(newPos) != Wall
            } yield newPos
          }
        }

        val distances = BFS.traverse(graphTraversal).distances
        fromPortalPos -> portalPoss.view.flatMap(toPortalPos => distances.get(toPortalPos).map(toPortalPos -> _)).toMap
      }).toMap


    case class Node(pos: Pos, level: Int)

    val graphSearch = new GraphSearch[Node] with TargetNode[Node] {
      override val startNode: Node = Node(grid.posOf(startPortal), 0)

      override def neighbors(node: Node): IterableOnce[(Node, Int)] = {
        val Node(pos, level) = node

        val moveNeighbors = for {
          (newPos, distance) <- portalPosNeighbors(pos).iterator
        } yield Node(newPos, level) -> distance

        val portalNeighbors = grid(pos) match {
          case NormalPortal(portal) =>
            val otherPos = (portals(portal) - pos).head
            if (innerBox.contains(pos))
              Iterator(Node(otherPos, level + 1) -> 1)
            else if (level > 0)
              Iterator(Node(otherPos, level - 1) -> 1)
            else
              Iterator.empty
          case _ => Iterator.empty
        }

        moveNeighbors ++ portalNeighbors
      }

      override val targetNode: Node = Node(grid.posOf(targetPortal), 0)
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  case class Input(grid: Grid[Tile], portals: Map[Portal, Set[Pos]])

  def parseGrid(input: String): Input = {
    val charGrid = input.linesIterator.map(_.toVector).toVector

    val wallGrid = charGrid.mapGrid({
      case '.' => Open
      case _ => Wall
    })

    // TODO: refactor Grid iterations
    val posPortal = (for {
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
    } yield pos -> (Portal(label): Portal)).toMap

    val grid = posPortal.foldLeft(wallGrid)({ case (grid, (pos, portal)) =>
      grid.updatedGrid(pos, portal)
    })

    val portals = posPortal.groupMap(_._2)(_._1).transform({ (_, v) => v.toSet })

    Input(grid, portals)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(steps(parseGrid(input)))
    println(stepsRecursive(parseGrid(input)))

    // 700 - too high
  }
}
