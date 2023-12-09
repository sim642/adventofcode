package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.{Grid, NumberTheory}
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{AStar, GraphSearch, Heuristic, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits.*

import scala.collection.immutable.BitSet

object Day24 {

  case class Input(size: Pos,
                   wall: Set[Pos],
                   up: Map[Int, BitSet],
                   down: Map[Int, BitSet],
                   left: Map[Int, BitSet],
                   right: Map[Int, BitSet])

  trait Part {

    val stages: Int

    def fewestMinutes(input: Input): Int = {
      val Input(size, wall, up, down, left, right) = input
      val max = size - Pos(1, 1)
      val entrance = Pos(1, 0)
      val exit = max - Pos(1, 0)
      val innerSize = Pos(size.x - 2, size.y - 2)
      val timeModulus = NumberTheory.lcm(innerSize.x, innerSize.y)

      // precompute occupied by axis and modulo time
      val timeHorizontal = Array.tabulate(innerSize.x, innerSize.y)((time, y) =>
        right(y + 1).map(x => (x - 1 + time) %+ innerSize.x + 1) ++
          left(y + 1).map(x => (x - 1 - time) %+ innerSize.x + 1)
      )
      val timeVertical = Array.tabulate(innerSize.y, innerSize.x)((time, x) =>
        down(x + 1).map(y => (y - 1 + time) %+ innerSize.y + 1) ++
          up(x + 1).map(y => (y - 1 - time) %+ innerSize.y + 1)
      )

      def isFree(pos: Pos, time: Int): Boolean = {
        !timeHorizontal(time % innerSize.x)(pos.y - 1).contains(pos.x) &&
          !timeVertical(time % innerSize.y)(pos.x - 1).contains(pos.y)
      }

      case class State(pos: Pos, time: Int, stage: Int) {
        def stageTarget: Pos = if (stage % 2 == 0) exit else entrance

        def steps: Iterator[State] = {
          for {
            offset <- Pos.axisOffsets.iterator ++ Iterator.single(Pos.zero)
            newPos = pos + offset
            if Pos.zero <= newPos && newPos <= max
            if !wall(newPos)
            newTime = (time + 1) % timeModulus
            if newPos == entrance || newPos == exit || isFree(newPos, newTime)
            newStage = if (pos == stageTarget) stage + 1 else stage
          } yield State(newPos, newTime, newStage)
        }
      }

      val graphSearch = new GraphSearch[State] with UnitNeighbors[State] with Heuristic[State] {
        override val startNode: State = State(entrance, 0, 0)

        override def unitNeighbors(state: State): IterableOnce[State] = state.steps

        override def isTargetNode(state: State, dist: Int): Boolean = state.pos == exit && state.stage == stages - 1

        private val entranceExitDist = entrance manhattanDistance exit

        override def heuristic(state: State): Int =
          (state.pos manhattanDistance state.stageTarget) + (stages - state.stage - 1) * entranceExitDist
      }

      AStar.search(graphSearch).target.get._2
    }
  }

  object Part1 extends Part {
    override val stages: Int = 1
  }

  object Part2 extends Part {
    override val stages: Int = 3
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInput(input: String): Input = {
    val grid = parseGrid(input)
    val size = Pos(grid.head.size, grid.size)

    def findChars(arrow: Char): Set[Pos] = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == arrow
    } yield Pos(x, y)).toSet

    val wall = findChars('#')
    val up = findChars('^').groupMap(_.x)(_.y).view.mapValues(_.to(BitSet)).toMap.withDefaultValue(BitSet.empty)
    val down = findChars('v').groupMap(_.x)(_.y).view.mapValues(_.to(BitSet)).toMap.withDefaultValue(BitSet.empty)
    val left = findChars('<').groupMap(_.y)(_.x).view.mapValues(_.to(BitSet)).toMap.withDefaultValue(BitSet.empty)
    val right = findChars('>').groupMap(_.y)(_.x).view.mapValues(_.to(BitSet)).toMap.withDefaultValue(BitSet.empty)

    Input(size, wall, up, down, left, right)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.fewestMinutes(parseInput(input)))
    println(Part2.fewestMinutes(parseInput(input)))
  }
}
