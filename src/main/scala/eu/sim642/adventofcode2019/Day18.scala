package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, UnitNeighbors}

object Day18 {

  def collectKeysSteps(input: Input): Int = {

    case class KeyNode(pos: Pos)(val pathDoors: Set[Pos], val pathKeys: Set[Pos])

    val keyNeighbors: Map[Pos, Map[Pos, (KeyNode, Int)]] = (input.entrances ++ input.keys.keySet).view.map({ fromPos =>

      val graphTraversal = new GraphTraversal[KeyNode] with UnitNeighbors[KeyNode] {
        override val startNode: KeyNode = KeyNode(fromPos)(Set.empty, Set.empty)

        override def unitNeighbors(keyNode: KeyNode): IterableOnce[KeyNode] = {
          val KeyNode(pos) = keyNode
          val pathDoors = keyNode.pathDoors
          val pathKeys = keyNode.pathKeys

          if (false)//if (pos != fromPos && input.keys.contains(pos))
            Iterator.empty
          else {
            for {
              offset <- Pos.axisOffsets.iterator
              newPos = pos + offset
              if !input.walls(newPos)
              newPathDoors = if (input.doors.contains(newPos)) pathDoors + newPos else pathDoors
              newPathKeys = if (input.keys.contains(newPos)) pathKeys + newPos else pathKeys
            } yield KeyNode(newPos)(newPathDoors, newPathKeys)
          }
        }
      }

      val distances = BFS.traverse(graphTraversal).distances
      val keyDistances = distances.filter({ case (KeyNode(toPos), _) =>
        toPos != fromPos && input.keys.keySet.contains(toPos)
      }).groupMap(_._1.pos)(identity).transform({ case (_, x) =>
        assert(x.sizeIs == 1)
        x.head
      })

      fromPos -> keyDistances
    }).toMap

    //println(keyNeighbors)

    case class Node(poss: Seq[Pos], keys: Map[Pos, Char], doors: Map[Pos, Char])

    val graphSearch = new GraphSearch[Node] {
      override val startNode: Node = Node(input.entrances.toSeq, input.keys, input.doors)

      override def neighbors(node: Node): IterableOnce[(Node, Int)] = {
        val Node(poss, keys, doors) = node

        (for {
          (pos, posI) <- poss.view.zipWithIndex
          //(keyPos, key) <- keys.iterator

          (keyPos, (keyNode, distance)) <- keyNeighbors(pos)
          pathDoors = keyNode.pathDoors
          pathKeys = keyNode.pathKeys
          if keys.contains(keyPos)
          if (doors.keySet intersect pathDoors).isEmpty
          if (keys.keySet intersect pathKeys).sizeIs == 1

          key = keys(keyPos)
          newPoss = poss.updated(posI, keyPos)
          newKeys = keys - keyPos
          newDoors = doors.filterNot(_._2 == key.toUpper)
        } yield Node(newPoss, newKeys, newDoors) -> distance)// .tapEach(p => println("NEIGH", p))
      }

      override def isTargetNode(node: Node, dist: Int): Boolean = {
        //println("TARGET", node, dist)
        node.keys.isEmpty
      }
    }

    val value = Dijkstra.search(graphSearch)
    //println(value.nodes)
    value.target.get._2
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
    //println(collectKeysStepsSplit(parseInput(input)))
  }
}
