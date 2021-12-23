package eu.sim642.adventofcode2021

import eu.sim642.adventofcode2021.Day23.Amphipod.*
import eu.sim642.adventofcode2021.Day23.Loc.*
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

object Day23 {

  enum Amphipod(val energy: Int) {
    case Amber extends Amphipod(1)
    case Bronze extends Amphipod(10)
    case Copper extends Amphipod(100)
    case Desert extends Amphipod(1000)
  }

  enum Loc(val pos: Pos) {
    case AL extends Loc(Pos(3, 3))
    case AH extends Loc(Pos(3, 2))
    case BL extends Loc(Pos(5, 3))
    case BH extends Loc(Pos(5, 2))
    case CL extends Loc(Pos(7, 3))
    case CH extends Loc(Pos(7, 2))
    case DL extends Loc(Pos(9, 3))
    case DH extends Loc(Pos(9, 2))

    case HLL extends Loc(Pos(1, 1))
    case HL extends Loc(Pos(2, 1))
    case HAB extends Loc(Pos(4, 1))
    case HBC extends Loc(Pos(6, 1))
    case HCD extends Loc(Pos(8, 1))
    case HR extends Loc(Pos(10, 1))
    case HRR extends Loc(Pos(11, 1))
  }

  val hallwayLocs = Seq(HLL, HL, HAB, HBC, HCD, HR, HRR)
  val roomLocs = Map(
    Amber -> Seq(AL, AH),
    Bronze -> Seq(BL, BH),
    Copper -> Seq(CL, CH),
    Desert -> Seq(DL, DH),
  )

  type State = Map[Loc, Amphipod]

  private val map =
    """#############
      |#...........#
      |###.#.#.#.###
      |  #.#.#.#.#
      |  #########""".stripMargin

  private val mapGrid: Grid[Boolean] = map.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  def freePathLength(occupied: Set[Pos], fromLoc: Loc, toLoc: Loc): Option[Int] = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = fromLoc.pos

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if !mapGrid(newPos)
          if !occupied.contains(newPos)
        } yield newPos
      }

      override val targetNode: Pos = toLoc.pos
    }

    BFS.search(graphSearch).target.map(_._2)
  }

  def minimumOrganizeEnergy(initialState: State): Int = {

    val graphSearch = new GraphSearch[State] with TargetNode[State] {
      override val startNode: State = initialState

      override def neighbors(state: State): IterableOnce[(State, Int)] = {

        val occupied = state.keys.map(_.pos).toSet

        def room2hallway(fromLoc: Loc): Seq[(Loc, Int)] = {
          for {
            toLoc <- hallwayLocs
            length <- freePathLength(occupied, fromLoc, toLoc)
          } yield toLoc -> length
        }

        def hallway2room(fromLoc: Loc, amphipod: Amphipod): Seq[(Loc, Int)] = {
          if (roomLocs(amphipod).flatMap(state.get).forall(_ == amphipod)) {
            for {
              toLoc <- roomLocs(amphipod)
              length <- freePathLength(occupied, fromLoc, toLoc)
            } yield toLoc -> length
          }
          else
            Seq.empty
        }

        for {
          (fromLoc, amphipod) <- state.iterator
          (toLoc, length) <- if (hallwayLocs.contains(fromLoc)) hallway2room(fromLoc, amphipod) else room2hallway(fromLoc)
          newState = state - fromLoc + (toLoc -> amphipod)
        } yield newState -> amphipod.energy * length
      }

      override val targetNode: State = Map(
        AH -> Amber,
        AL -> Amber,
        BH -> Bronze,
        BL -> Bronze,
        CH -> Copper,
        CL -> Copper,
        DH -> Desert,
        DL -> Desert,
      )
    }

    Dijkstra.search(graphSearch).target.get._2
  }


  def parseAmphipod(s: String): Amphipod = s match {
    case "A" => Amber
    case "B" => Bronze
    case "C" => Copper
    case "D" => Desert
  }

  private val stateRegex =
    """#############
      |#\.\.\.\.\.\.\.\.\.\.\.#
      |###(.)#(.)#(.)#(.)###
      |  #(.)#(.)#(.)#(.)#
      |  #########""".stripMargin.r

  def parseState(input: String): State = input match {
    case stateRegex(ah, bh, ch, dh, al, bl, cl, dl) =>
      Map(
        AH -> parseAmphipod(ah),
        AL -> parseAmphipod(al),
        BH -> parseAmphipod(bh),
        BL -> parseAmphipod(bl),
        CH -> parseAmphipod(ch),
        CL -> parseAmphipod(cl),
        DH -> parseAmphipod(dh),
        DL -> parseAmphipod(dl),
      )
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minimumOrganizeEnergy(parseState(input)))
  }
}
