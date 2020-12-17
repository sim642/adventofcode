package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.pos.{Pos, Pos3, Pos4, PosOps}

object Day17 {

  sealed trait Part {
    type A <: PosOps[A]

    val allOffsets: Seq[A]

    private def neighbors(pos: A): Iterator[A] = allOffsets.iterator.map(pos + _)

    private def possibles(state: Set[A]): Set[A] = state ++ state.flatMap(neighbors)

    def step(state: Set[A]): Set[A] = {
      possibles(state)
        .filter({ pos =>
          val active = neighbors(pos).count(state)
          state.contains(pos) match {
            case true if active == 2 || active == 3 => true
            case true => false
            case false if active == 3 => true
            case false => false
          }
        })
    }

    def embedPos(pos: Pos): A

    def fromGrid(grid: Grid[Boolean]): Set[A] = {
      (for {
        (row, y) <- grid.zipWithIndex
        (cell, x) <- row.zipWithIndex
        if cell
        pos = Pos(x, y)
      } yield embedPos(pos)).toSet
    }

    def countCubesBooted(grid: Grid[Boolean], steps: Int = 6): Int = {
      val initialState = fromGrid(grid)
      val finalState = Iterator.iterate(initialState)(step)(steps)
      finalState.size
    }
  }

  object Part1 extends Part {
    override type A = Pos3

    override val allOffsets: Seq[Pos3] =
      for {
        z <- -1 to 1
        y <- -1 to 1
        x <- -1 to 1
        pos = Pos3(x, y, z)
        if pos != Pos3.zero
      } yield pos

    override def embedPos(pos: Pos): Pos3 = Pos3(pos.x, pos.y, 0)
  }

  object Part2 extends Part {
    override type A = Pos4

    override val allOffsets: Seq[Pos4] =
      for {
        w <- -1 to 1
        z <- -1 to 1
        y <- -1 to 1
        x <- -1 to 1
        pos = Pos4(x, y, z, w)
        if pos != Pos4.zero
      } yield pos

    override def embedPos(pos: Pos): Pos4 = Pos4(pos.x, pos.y, 0, 0)
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countCubesBooted(parseGrid(input)))
    println(Part2.countCubesBooted(parseGrid(input)))
  }
}
