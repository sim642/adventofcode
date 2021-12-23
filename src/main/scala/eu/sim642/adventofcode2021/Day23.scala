package eu.sim642.adventofcode2021

import eu.sim642.adventofcode2021.Day23.Amphipod.*
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

object Day23 {

  enum Amphipod(val energy: Int) {
    case Amber extends Amphipod(1)
    case Bronze extends Amphipod(10)
    case Copper extends Amphipod(100)
    case Desert extends Amphipod(1000)
  }

  // TODO: restore part 1
  // TODO: optimize part 2

  private val map =
    """#############
      |#HH.H.H.H.HH#
      |###A#B#C#D###
      |  #A#B#C#D#
      |  #A#B#C#D#
      |  #A#B#C#D#
      |  #########""".stripMargin

  private val mapGrid = map.linesIterator.map(_.toVector).toVector

  val hallwayPoss: Set[Pos] = (for {
    (row, y) <- mapGrid.view.zipWithIndex
    (cell, x) <- row.view.zipWithIndex
    if cell == 'H'
  } yield Pos(x, y)).toSet

  val roomPoss: Map[Amphipod, Set[Pos]] = (for {
    (row, y) <- mapGrid.view.zipWithIndex
    (cell, x) <- row.view.zipWithIndex
    if "ABCD".contains(cell)
  } yield parseAmphipod(cell.toString) -> Pos(x, y)).toSet.groupMap(_._1)(_._2)

  val posRooms: Map[Pos, Amphipod] = for {
    (amphipod, poss) <- roomPoss
    pos <- poss
  } yield pos -> amphipod

  val allPoss: Set[Pos] = hallwayPoss ++ roomPoss.values.flatten

  type State = Map[Pos, Amphipod]

  case class FreePathData(length: Int, blockers: Set[Pos])
  private val freePathMap: collection.Map[Pos, collection.Map[Pos, FreePathData]] = {
    allPoss.view.map({ fromPos =>

      case class PathPos(pos: Pos)(val blockers: Set[Pos])

      val graphTraversal = new GraphTraversal[PathPos] with UnitNeighbors[PathPos] {
        override val startNode: PathPos = PathPos(fromPos)(Set.empty)

        override def unitNeighbors(pathPos: PathPos): IterableOnce[PathPos] = {
          val PathPos(pos) = pathPos
          val blockers = pathPos.blockers

          for {
            offset <- Pos.axisOffsets.iterator
            newPos = pos + offset
            if mapGrid(newPos) != '#'
            newBlockers = if (allPoss.contains(newPos)) blockers + newPos else blockers
          } yield PathPos(newPos)(newBlockers)
        }
      }

      val distances = BFS.traverse(graphTraversal).distances
      val distances2 =
        for {
          (pathPos@PathPos(pos), length) <- distances
          if allPoss.contains(pos)
        } yield pos -> FreePathData(length, pathPos.blockers)
      fromPos -> distances2
    }).toMap
  }

  def freePathLength(occupied: Set[Pos], fromPos: Pos, toPos: Pos): Option[Int] = {
    val FreePathData(length, blockers) = freePathMap(fromPos)(toPos)
    if (blockers.exists(occupied.contains))
      None
    else
      Some(length)
  }

  def minimumOrganizeEnergy(initialState: State): Int = {

    val graphSearch = new GraphSearch[State] with TargetNode[State] {
      override val startNode: State = initialState

      override def neighbors(state: State): IterableOnce[(State, Int)] = {

        val occupied = state.keySet

        def room2hallway(fromPos: Pos, amphipod: Amphipod): Set[(Pos, Int)] = {
          if (posRooms(fromPos) == amphipod && roomPoss(amphipod).flatMap(state.get).forall(_ == amphipod)) // forbid useless move out of final (correct) room
            Set.empty
          else {
            for {
              toPos <- hallwayPoss
              length <- freePathLength(occupied, fromPos, toPos)
            } yield toPos -> length
          }
        }

        def hallway2room(fromPos: Pos, amphipod: Amphipod): Set[(Pos, Int)] = {
          if (roomPoss(amphipod).flatMap(state.get).forall(_ == amphipod)) {
            for {
              toPos <- roomPoss(amphipod)
              length <- freePathLength(occupied, fromPos, toPos)
            } yield toPos -> length
          }
          else
            Set.empty
        }

        for {
          (fromPos, amphipod) <- state.iterator
          (toLoc, length) <- if (hallwayPoss.contains(fromPos)) hallway2room(fromPos, amphipod) else room2hallway(fromPos, amphipod)
          newState = state - fromPos + (toLoc -> amphipod)
        } yield newState -> amphipod.energy * length
      }

      override val targetNode: State = {
        (for {
          (row, y) <- mapGrid.view.zipWithIndex
          (cell, x) <- row.view.zipWithIndex
          if "ABCD".contains(cell)
        } yield Pos(x, y) -> parseAmphipod(cell.toString)).toMap
      }
    }

    Dijkstra.search(graphSearch).target.get._2
  }


  def parseAmphipod(s: String): Amphipod = s match {
    case "A" => Amber
    case "B" => Bronze
    case "C" => Copper
    case "D" => Desert
  }

  private val part2Lines =
    """  #D#C#B#A#
      |  #D#B#A#C#""".stripMargin

  def parseState(input: String): State = {
    val lines = input.linesIterator.toVector
    val lines2 = lines.take(3) ++ part2Lines.linesIterator.toVector ++ lines.drop(3)
    val grid = lines2.map(_.toVector)
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if "ABCD".contains(cell)
    } yield Pos(x, y) -> parseAmphipod(cell.toString)).toMap
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minimumOrganizeEnergy(parseState(input)))
  }
}
