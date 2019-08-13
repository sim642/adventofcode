package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.pos.Pos

object Day22 {

  implicit class OffsetPos(offset: Pos) {
    def right: Pos = Pos(offset.y, -offset.x)
    def left: Pos = Pos(-offset.y, offset.x)
    def reverse: Pos = Pos(-offset.x, -offset.y)
  }

  trait Part {
    sealed trait Status
    case object Clean extends Status
    case object Infected extends Status

    case class InfectionState(statuses: Map[Pos, Status], pos: Pos = Pos(0, 0), prevOffset: Pos = Pos(0, 1)) {
      def posStatus: Status = statuses(pos)

      def next(statuses: Map[Pos, Status], offset: Pos) = InfectionState(statuses, pos + offset, offset)
    }

    protected def nextState(state: InfectionState): InfectionState

    def parseMap(input: String): Map[Pos, Status] = {
      val grid = input.lines.map(_.map(_ == '#').toVector).toVector
      val (h, w) = (grid.size, grid(0).size)

      (for {
        y <- grid.indices
        x <- grid(y).indices
        if grid(y)(x)
      } yield Pos(x - w / 2, -(y - h / 2)) -> Infected).toMap.withDefaultValue(Clean)
    }

    def iterateBursts(input: String): Iterator[InfectionState] = Iterator.iterate(InfectionState(parseMap(input)))(nextState)

    protected val defaultBursts: Int
    protected val toBeInfectedStatus: Status

    def infectionBursts(input: String, bursts: Int = defaultBursts): Int = iterateBursts(input).take(bursts).count(_.posStatus == toBeInfectedStatus)
  }

  object Part1 extends Part {
    override protected def nextState(state: InfectionState): InfectionState = {
      val InfectionState(statuses, pos, prevOffset) = state

      state.posStatus match {
        case Clean =>
          state.next(statuses + (pos -> Infected), prevOffset.left)
        case Infected =>
          state.next(statuses - pos, prevOffset.right)
      }
    }

    override protected val defaultBursts: Int = 10000
    override protected val toBeInfectedStatus: Status = Clean
  }

  object Part2 extends Part {
    case object Weakened extends Status
    case object Flagged extends Status

    override protected def nextState(state: InfectionState): InfectionState = {
      val InfectionState(statuses, pos, prevOffset) = state

      state.posStatus match {
        case Clean =>
          state.next(statuses + (pos -> Weakened), prevOffset.left)
        case Weakened =>
          state.next(statuses + (pos -> Infected), prevOffset)
        case Infected =>
          state.next(statuses + (pos -> Flagged), prevOffset.right)
        case Flagged =>
          state.next(statuses - pos, prevOffset.reverse)
      }
    }

    override protected val defaultBursts: Int = 10000000
    override protected val toBeInfectedStatus: Status = Weakened
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.infectionBursts(input))
    println(Part2.infectionBursts(input))
  }
}
