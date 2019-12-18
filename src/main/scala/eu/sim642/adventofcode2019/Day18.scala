package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, UnitNeighbors}

object Day18 {

  // TODO: move to library, reuse elsewhere?
  implicit class DisjointSetOps[A](thisSet: Set[A]) {
    def disjoint(thatSet: Set[A]): Boolean = !thisSet.exists(thatSet)
  }

  def collectKeysSteps(input: Input): Int = {
    // precompute BFS between keys (and entrances for starting, one way)
    case class PathData(distance: Int, pathDoors: Set[Pos], pathKeys: Set[Pos])

    val keyNeighbors: Map[Pos, Map[Pos, PathData]] = (input.entrances.view ++ input.keys.keySet.view).map({ fromPos =>
        // TODO: remove hack to keep sets in node, generalize BFS to arbitrary data keeping, not just distance
        case class PathPos(pos: Pos)(val pathDoors: Set[Pos], val pathKeys: Set[Pos])

        val graphTraversal = new GraphTraversal[PathPos] with UnitNeighbors[PathPos] {
          override val startNode: PathPos = PathPos(fromPos)(Set.empty, Set.empty)

          override def unitNeighbors(pathPos: PathPos): IterableOnce[PathPos] = {
            val PathPos(pos) = pathPos
            val pathDoors = pathPos.pathDoors
            val pathKeys = pathPos.pathKeys

            for {
              offset <- Pos.axisOffsets.iterator
              newPos = pos + offset
              if !input.walls(newPos)
              newPathDoors = if (input.doors.contains(newPos)) pathDoors + newPos else pathDoors
              newPathKeys = if (input.keys.contains(newPos)) pathKeys + newPos else pathKeys
            } yield PathPos(newPos)(newPathDoors, newPathKeys)
          }
        }

        val distances = BFS.traverse(graphTraversal).distances
        val keyDistances = distances.filter({ case (PathPos(toPos), _) =>
            toPos != fromPos && input.keys.keySet.contains(toPos)
          })
        val toPosPathDatas = keyDistances.groupMapReduce(_._1.pos)({ case (pathPos@PathPos(toPos), distance) =>
              PathData(distance, pathPos.pathDoors, pathPos.pathKeys - toPos)
            })((x, _) => x) // TODO: check that reduce actually never gets called, should be unique positions

        fromPos -> toPosPathDatas
      }).toMap


    // use precomputed BFS for Dijkstra on only keys (and entrances for starting)
    case class Node(robots: Seq[Pos], missingKeys: Map[Pos, Char], doors: Map[Pos, Char])

    val graphSearch = new GraphSearch[Node] {
      override val startNode: Node = Node(input.entrances.toSeq, input.keys, input.doors)

      override def neighbors(node: Node): IterableOnce[(Node, Int)] = {
        val Node(robots, missingKeys, doors) = node

        for {
          (robot, robotI) <- robots.view.zipWithIndex

          (key, PathData(distance, pathDoors, pathKeys)) <- keyNeighbors(robot)
          if missingKeys.contains(key) // key is still needed
          if doors.keySet disjoint pathDoors // no doors in the way to key
          if missingKeys.keySet disjoint pathKeys // no other keys on the way to key

          keyChar = missingKeys(key)
          newRobots = robots.updated(robotI, key)
          newMissingKeys = missingKeys - key
          newDoors = doors.filterNot(_._2 == keyChar.toUpper) // TODO: optimize removal
        } yield Node(newRobots, newMissingKeys, newDoors) -> distance
      }

      override def isTargetNode(node: Node, dist: Int): Boolean = node.missingKeys.isEmpty
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  def splitEntrance(input: Input): Input = {
    assume(input.entrances.size == 1)
    val entrance = input.entrances.head

    val newEntrances = Pos.diagonalOffsets.map(entrance + _).toSet
    val newWalls = (entrance +: Pos.axisOffsets.map(entrance + _)).foldLeft(input.walls)((walls, pos) => walls.updatedGrid(pos, true))

    input.copy(entrances = newEntrances, walls = newWalls)
  }

  def collectKeysStepsSplit(input: Input): Int = collectKeysSteps(splitEntrance(input))

  case class Input(walls: Grid[Boolean],
                   entrances: Set[Pos],
                   keys: Map[Pos, Char],
                   doors: Map[Pos, Char])

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInput(input: String): Input = {
    val grid = parseGrid(input)

    val walls = grid.mapGrid(_ == '#')
    // TODO: refactor Grid iterations
    val entrances = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '@'
    } yield Pos(x, y)).toSet
    val keys = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell.isLetter && cell.isLower
    } yield Pos(x, y) -> cell).toMap
    val doors = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell.isLetter && cell.isUpper
    } yield Pos(x, y) -> cell).toMap

    Input(walls, entrances, keys, doors)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(collectKeysSteps(parseInput(input)))
    println(collectKeysStepsSplit(parseInput(input)))
  }
}
