package eu.sim642.adventofcode2021

import eu.sim642.adventofcode2021.Day23.Amphipod.*
import eu.sim642.adventofcode2021.Day23.Loc.*
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

import scala.collection.mutable

object Day23 {

  enum Amphipod(val energy: Int) {
    case Amber extends Amphipod(1)
    case Bronze extends Amphipod(10)
    case Copper extends Amphipod(100)
    case Desert extends Amphipod(1000)
  }

  // TODO: restore part 1
  // TODO: optimize part 2

  enum Loc(val pos: Pos) {
    case AL2 extends Loc(Pos(3, 5))
    case AL1 extends Loc(Pos(3, 4))
    case AL extends Loc(Pos(3, 3))
    case AH extends Loc(Pos(3, 2))
    case BL2 extends Loc(Pos(5, 5))
    case BL1 extends Loc(Pos(5, 4))
    case BL extends Loc(Pos(5, 3))
    case BH extends Loc(Pos(5, 2))
    case CL2 extends Loc(Pos(7, 5))
    case CL1 extends Loc(Pos(7, 4))
    case CL extends Loc(Pos(7, 3))
    case CH extends Loc(Pos(7, 2))
    case DL2 extends Loc(Pos(9, 5))
    case DL1 extends Loc(Pos(9, 4))
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
    Amber -> Seq(AL2, AL1, AL, AH),
    Bronze -> Seq(BL2, BL1, BL, BH),
    Copper -> Seq(CL2, CL1, CL, CH),
    Desert -> Seq(DL2, DL1, DL, DH),
  )
  val locRooms = for {
    (amphipod, locs) <- roomLocs
    loc <- locs
  } yield loc -> amphipod

  type State = Map[Loc, Amphipod]

  private val map =
    """#############
      |#...........#
      |###.#.#.#.###
      |  #.#.#.#.#
      |  #.#.#.#.#
      |  #.#.#.#.#
      |  #########""".stripMargin

  private val mapGrid: Grid[Boolean] = map.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  case class FreePathData(length: Int, blockers: Set[Loc])
  private val freePathMap: collection.Map[Loc, collection.Map[Loc, FreePathData]] = {
    Loc.values.view.map({ fromLoc =>

      case class PathPos(pos: Pos)(val blockers: Set[Loc])

      val graphTraversal = new GraphTraversal[PathPos] with UnitNeighbors[PathPos] {
        override val startNode: PathPos = PathPos(fromLoc.pos)(Set.empty)

        override def unitNeighbors(pathPos: PathPos): IterableOnce[PathPos] = {
          val PathPos(pos) = pathPos
          val blockers = pathPos.blockers

          for {
            offset <- Pos.axisOffsets.iterator
            newPos = pos + offset
            if !mapGrid(newPos)
            newBlockers = Loc.values.find(_.pos == newPos).map(blockers + _).getOrElse(blockers)
          } yield PathPos(newPos)(newBlockers)
        }
      }

      val distances = BFS.traverse(graphTraversal).distances
      val distances2 =
        for {
          (pathPos@PathPos(pos), length) <- distances
          loc <- Loc.values.find(_.pos == pos)
        } yield loc -> FreePathData(length, pathPos.blockers)
      fromLoc -> distances2
    }).toMap
  }

  def freePathLength(occupied: Set[Pos], fromLoc: Loc, toLoc: Loc): Option[Int] = {
    val FreePathData(length, blockers) = freePathMap(fromLoc)(toLoc)
    if (blockers.exists(b => occupied.contains(b.pos)))
      None
    else
      Some(length)
  }

  def minimumOrganizeEnergy(initialState: State): Int = {

    val graphSearch = new GraphSearch[State] with TargetNode[State] {
      override val startNode: State = initialState

      override def neighbors(state: State): IterableOnce[(State, Int)] = {

        val occupied = state.keys.map(_.pos).toSet

        def room2hallway(fromLoc: Loc, amphipod: Amphipod): Seq[(Loc, Int)] = {
          if (locRooms(fromLoc) == amphipod && roomLocs(amphipod).flatMap(state.get).forall(_ == amphipod)) // forbid useless move out of final (correct) room
            Seq.empty
          else {
            for {
              toLoc <- hallwayLocs
              length <- freePathLength(occupied, fromLoc, toLoc)
            } yield toLoc -> length
          }
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
          (toLoc, length) <- if (hallwayLocs.contains(fromLoc)) hallway2room(fromLoc, amphipod) else room2hallway(fromLoc, amphipod)
          newState = state - fromLoc + (toLoc -> amphipod)
        } yield newState -> amphipod.energy * length
      }

      override val targetNode: State = Map(
        AH -> Amber,
        AL -> Amber,
        AL1 -> Amber,
        AL2 -> Amber,
        BH -> Bronze,
        BL -> Bronze,
        BL1 -> Bronze,
        BL2 -> Bronze,
        CH -> Copper,
        CL -> Copper,
        CL1 -> Copper,
        CL2 -> Copper,
        DH -> Desert,
        DL -> Desert,
        DL1 -> Desert,
        DL2 -> Desert,
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
        AL -> Desert,
        AL1 -> Desert,
        AL2 -> parseAmphipod(al),
        BH -> parseAmphipod(bh),
        BL -> Copper,
        BL1 -> Bronze,
        BL2 -> parseAmphipod(bl),
        CH -> parseAmphipod(ch),
        CL -> Bronze,
        CL1 -> Amber,
        CL2 -> parseAmphipod(cl),
        DH -> parseAmphipod(dh),
        DL -> Amber,
        DL1 -> Copper,
        DL2 -> parseAmphipod(dl),
      )
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minimumOrganizeEnergy(parseState(input)))
  }
}
