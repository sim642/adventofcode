package eu.sim642.adventofcode2022

import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.IterableImplicits.*
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.SeqImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._

object Day24 {

  case class Input(size: Pos,
                   wall: Set[Pos],
                   up: Map[Int, Set[Int]],
                   down: Map[Int, Set[Int]],
                   left: Map[Int, Set[Int]],
                   right: Map[Int, Set[Int]])

  trait Part {

    val stages: Int

    def fewestMinutes(input: Input): Int = {
      val Input(size, wall, up, down, left, right) = input
      val max = size - Pos(1, 1)

      def isFree(pos: Pos, time: Int): Boolean = {
        val r = right(pos.y).map(x => (x - 1 + time) %+ (size.x - 2) + 1)
        val l = left(pos.y).map(x => (x - 1 - time) %+ (size.x - 2) + 1)
        val d = down(pos.x).map(y => (y - 1 + time) %+ (size.y - 2) + 1)
        val u = up(pos.x).map(y => (y - 1 - time) %+ (size.y - 2) + 1)
        //println(r)
        //println(l)
        //println(d)
        //println(u)
        !r.contains(pos.x) &&
          !l.contains(pos.x) &&
          !d.contains(pos.y) &&
          !u.contains(pos.y)
      }

      //println(isFree(Pos(1, 2), 3))

      case class State(pos: Pos, time: Int, stage: Int) {

        def steps: Iterator[State] = {
          //println(this)
          for {
            offset <- Pos.axisOffsets.iterator ++ Iterator.single(Pos.zero)
            newPos = pos + offset
            if Pos.zero <= newPos && newPos <= max
            if !wall(newPos)
            if isFree(newPos, time + 1)
            newStage = stage match {
              case 0 if newPos == max - Pos(1, 0) => 1
              case 1 if newPos == Pos(1, 0) => 2
              case 2 if newPos == max - Pos(1, 0) => 3
              case stage => stage
            }
          } yield State(newPos, time + 1, newStage)
        }
      }

      val graphSearch = new GraphSearch[State] with UnitNeighbors[State] {
        override val startNode: State = State(Pos(1, 0), 0, 0)

        override def unitNeighbors(state: State): IterableOnce[State] = state.steps

        override def isTargetNode(state: State, dist: Int): Boolean = state.pos == max - Pos(1, 0) && state.stage == stages
      }

      BFS.search(graphSearch).target.get._2
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
    val up = findChars('^').groupMap(_.x)(_.y).withDefaultValue(Set.empty)
    val down = findChars('v').groupMap(_.x)(_.y).withDefaultValue(Set.empty)
    val left = findChars('<').groupMap(_.y)(_.x).withDefaultValue(Set.empty)
    val right = findChars('>').groupMap(_.y)(_.x).withDefaultValue(Set.empty)

    Input(size, wall, up, down, left, right)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.fewestMinutes(parseInput(input)))
    //println(Part2.fewestMinutes(parseInput(input)))
  }
}
