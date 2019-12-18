package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, UnitNeighbors}

object Day18 {

  def collectKeysSteps(input: Input): Int = {

    case class Node(poss: Seq[Pos], keys: Map[Pos, Char], doors: Map[Pos, Char])

    val graphSearch = new GraphSearch[Node] {
      override val startNode: Node = Node(input.entrances.toSeq, input.keys, input.doors)

      override def neighbors(node: Node): IterableOnce[(Node, Int)] = {
        val Node(poss, keys, doors) = node

        val distances = poss.view.zipWithIndex.map({ case (pos, posI) =>
          val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
            override val startNode: Pos = pos

            override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
              if (keys.contains(pos))
                Iterator.empty
              else {
                for {
                  offset <- Pos.axisOffsets
                  newPos = pos + offset
                  if !input.walls(newPos)
                  if !doors.contains(newPos)
                } yield newPos
              }
            }
          }

          BFS.traverse(graphTraversal).distances.map({ case (pos, distance) =>
            pos -> (posI, distance)
          })
        }).reduce(_ ++ _)

        for {
          (keyPos, key) <- keys.iterator
          (posI, distance) <- distances.get(keyPos)
          newPoss = poss.updated(posI, keyPos)
          newKeys = keys - keyPos
          newDoors = doors.filterNot(_._2 == key.toUpper)
        } yield Node(newPoss, newKeys, newDoors) -> distance
      }

      override def isTargetNode(node: Node, dist: Int): Boolean = node.keys.isEmpty
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
