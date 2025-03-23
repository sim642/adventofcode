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

  sealed trait Part {
    protected lazy val template: String // lazy val to avoid trait initialization order problems

    private val templateGrid = parseGrid(template)

    private val hallways: Set[Pos] = (for {
      (row, y) <- templateGrid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == 'H'
    } yield Pos(x, y)).toSet

    private val room2amphipod: Map[Pos, Amphipod] = grid2state(templateGrid)

    private val amphipod2rooms: Map[Amphipod, Set[Pos]] = room2amphipod.toSet.groupMap(_._2)(_._1)


    private case class PathData(length: Int, pathPoss: Set[Pos])

    // https://en.wikipedia.org/wiki/Transit_node_routing
    private val posNeighbors: collection.Map[Pos, collection.Map[Pos, PathData]] = {
      val allPoss: Set[Pos] = hallways ++ room2amphipod.keySet

      allPoss.view.map({ fromPos =>

        case class PathPos(pos: Pos)(val pathPoss: Set[Pos])

        val graphTraversal = new GraphTraversal[PathPos] with UnitNeighbors[PathPos] {
          override val startNode: PathPos = PathPos(fromPos)(Set.empty)

          override def unitNeighbors(pathPos: PathPos): IterableOnce[PathPos] = {
            val PathPos(pos) = pathPos
            val pathPoss = pathPos.pathPoss
            for {
              offset <- Pos.axisOffsets.iterator
              newPos = pos + offset
              if templateGrid(newPos) != '#'
              newPathPoss = if (allPoss.contains(newPos)) pathPoss + newPos else pathPoss
            } yield PathPos(newPos)(newPathPoss)
          }
        }

        val distances = BFS.traverse(graphTraversal).distances
        val allPosDistances =
          for {
            (pathPos@PathPos(toPos), length) <- distances
            if allPoss.contains(toPos)
          } yield toPos -> PathData(length, pathPos.pathPoss)
        fromPos -> allPosDistances
      }).toMap
    }


    protected type State = Map[Pos, Amphipod]

    protected def minimumOrganizeEnergy(initialState: State): Int = {

      val graphSearch = new GraphSearch[State] with TargetNode[State] {
        override val startNode: State = initialState

        override def neighbors(state: State): IterableOnce[(State, Int)] = {
          val amphipodRoomUniform: Map[Amphipod, Boolean] = amphipod2rooms.transform((amphipod, rooms) =>
            rooms.flatMap(state.get).forall(_ == amphipod)
          )

          val occupied = state.keySet

          def room2hallway(fromPos: Pos, amphipod: Amphipod): Set[(Pos, Int)] = {
            if (room2amphipod(fromPos) == amphipod && amphipodRoomUniform(amphipod)) // forbid useless move out of final (correct) room
              Set.empty
            else {
              (for {
                toPos <- hallways.view // slower without view for some reason
                PathData(length, pathPoss) = posNeighbors(fromPos)(toPos)
                if !pathPoss.exists(occupied.contains)
              } yield toPos -> length).toSet
            }
          }

          def hallway2room(fromPos: Pos, amphipod: Amphipod): Set[(Pos, Int)] = {
            if (amphipodRoomUniform(amphipod)) {
              val toPos = amphipod2rooms(amphipod).filterNot(occupied).maxBy(_.y) // only try furthest down unoccupied, others would uselessly block
              val PathData(length, pathPoss) = posNeighbors(fromPos)(toPos)
              if (!pathPoss.exists(occupied.contains))
                Set(toPos -> length)
              else
                Set.empty
            }
            else
              Set.empty
          }

          for {
            (fromPos, amphipod) <- state.iterator
            (toPos, length) <- (if (hallways.contains(fromPos)) hallway2room else room2hallway)(fromPos, amphipod)
            newState = state - fromPos + (toPos -> amphipod)
          } yield newState -> amphipod.energy * length
        }

        override val targetNode: State = grid2state(templateGrid)
      }

      Dijkstra.search(graphSearch).target.get._2
    }

    protected def grid2state(grid: Grid[Char]): State = {
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        amphipod <- parseAmphipod(cell)
      } yield Pos(x, y) -> amphipod).toMap
    }

    def minimumOrganizeEnergy(grid: Grid[Char]): Int = {
      minimumOrganizeEnergy(grid2state(grid))
    }
  }

  object Part1 extends Part {
    override protected lazy val template: String =
      """#############
        |#HH.H.H.H.HH#
        |###A#B#C#D###
        |  #A#B#C#D#
        |  #########""".stripMargin
  }

  object Part2 extends Part {
    override protected lazy val template: String =
      """#############
        |#HH.H.H.H.HH#
        |###A#B#C#D###
        |  #A#B#C#D#
        |  #A#B#C#D#
        |  #A#B#C#D#
        |  #########""".stripMargin

    private val extraLines =
      """  #D#C#B#A#
        |  #D#B#A#C#""".stripMargin

    private val extraGrid = parseGrid(extraLines)

    override def minimumOrganizeEnergy(grid: Grid[Char]): Int = {
      val (gridInit, gridTail) = grid.splitAt(3)
      val grid2 = gridInit ++ extraGrid ++ gridTail
      super.minimumOrganizeEnergy(grid2)
    }
  }


  def parseAmphipod(c: Char): Option[Amphipod] = c match {
    case 'A' => Some(Amber)
    case 'B' => Some(Bronze)
    case 'C' => Some(Copper)
    case 'D' => Some(Desert)
    case _ => None
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.minimumOrganizeEnergy(parseGrid(input)))
    println(Part2.minimumOrganizeEnergy(parseGrid(input)))
  }
}
