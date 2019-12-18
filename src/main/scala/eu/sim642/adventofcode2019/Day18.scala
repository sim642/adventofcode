package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, UnitNeighbors}

object Day18 {

  def collectKeysSteps(input: Input): Int = {

    case class Node(pos: Pos, keys: Map[Pos, Char], doors: Map[Pos, Char])

    val graphSearch = new GraphSearch[Node] {
      override val startNode: Node = Node(input.entrance, input.keys, input.doors)

      override def neighbors(node: Node): IterableOnce[(Node, Int)] = {
        val Node(pos, keys, doors) = node

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

        val distances = BFS.traverse(graphTraversal).distances

        for {
          (keyPos, key) <- keys.iterator
          distance <- distances.get(keyPos)
          newKeys = keys - keyPos
          newDoors = doors.filterNot(_._2 == key.toUpper)
        } yield Node(keyPos, newKeys, newDoors) -> distance
      }

      override def isTargetNode(node: Node, dist: Int): Boolean = node.keys.isEmpty
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  def collectKeysStepsParallel(input: Input): Int = {
    // TODO: reduce duplication

    case class Node(poss: Seq[Pos], keys: Map[Pos, Char], doors: Map[Pos, Char])

    val entrances = Pos.diagonalOffsets.map(input.entrance + _)
    val walls = (input.entrance +: Pos.axisOffsets.map(input.entrance + _)).foldLeft(input.walls)((walls, pos) => walls.updatedGrid(pos, true))

    val graphSearch = new GraphSearch[Node] {
      override val startNode: Node = Node(entrances, input.keys, input.doors)

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
                  if !walls(newPos)
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

  case class Input(walls: Grid[Boolean],
                   entrance: Pos,
                   keys: Map[Pos, Char],
                   doors: Map[Pos, Char])

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInput(input: String): Input = {
    val grid = parseGrid(input)

    val walls = grid.mapGrid(_ == '#')
    val entrance = grid.posOf('@')
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

    Input(walls, entrance, keys, doors)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(collectKeysSteps(parseInput(input)))
    println(collectKeysStepsParallel(parseInput(input)))
  }
}
