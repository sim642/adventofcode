package eu.sim642.adventofcode2017

import eu.sim642.adventofcode2017.Day3.Pos

object Day22 {

  implicit class OffsetPos(offset: Pos) {
    def right: Pos = Pos(offset.y, -offset.x)
    def left: Pos = Pos(-offset.y, offset.x)
  }

  case class InfectionState(infected: Set[Pos], pos: Pos = Pos(0, 0), prevOffset: Pos = Pos(0, 1)) {
    def next: InfectionState = {
      if (infected.contains(pos)) {
        val offset = prevOffset.right
        InfectionState(infected - pos, pos + offset, offset)
      }
      else {
        val offset = prevOffset.left
        InfectionState(infected + pos, pos + offset, offset)
      }
    }
  }

  def parseMap(input: String): Set[Pos] = {
    val grid = input.lines.map(_.map(_ == '#').toVector).toVector
    val (h, w) = (grid.size, grid(0).size)

    (for {
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x)
    } yield Pos(x - w / 2, -(y - h / 2))).toSet
  }

  def iterateBursts(input: String): Iterator[InfectionState] = Iterator.iterate(InfectionState(parseMap(input)))(_.next)

  def infectionBursts(input: String, bursts: Int = 10000): Int = iterateBursts(input).take(bursts).count(state => !state.infected.contains(state.pos))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(infectionBursts(input))
  }
}
