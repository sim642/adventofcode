package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.pos.{Pos, Pos3, Pos4, PosOps}

object Day17 {

  sealed trait Part {
    type A <: PosOps[A]

    val allOffsets: Seq[A]

    def syms(pos: A): Int
    def syms2(pos: A): Int = 1 << syms(pos)
    def symKeep(pos: A): Boolean

    private def neighbors(pos: A): Iterator[A] = allOffsets.iterator.map(pos + _)

    def step(state: Set[A]): Set[A] = {
      println(docount(state))
      state.iterator
        .flatMap(pos =>
          neighbors(pos)
            .flatMap(neigh =>
              if (syms(pos) > syms(neigh))
                Iterator.fill(syms2(pos) / syms2(neigh))(neigh)
              else
                Iterator(neigh)
            )
        )
        .filter(symKeep)
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .collect({
          case (pos, 3) => pos
          case (pos, 2) if state(pos) => pos
        })
        .toSet
    }

    def embedPos(pos: Pos): A

    def fromGrid(grid: Grid[Boolean]): Set[A] = {
      (for {
        (row, y) <- grid.zipWithIndex.view
        (cell, x) <- row.zipWithIndex.view
        if cell
        pos = Pos(x, y)
      } yield embedPos(pos)).toSet
    }

    def countCubesBooted(grid: Grid[Boolean], steps: Int = 6): Int = {
      val initialState = fromGrid(grid)
      val finalState = Iterator.iterate(initialState)(step)(steps)
      docount(finalState)
    }

    private def docount(finalState: Set[A]) = {
      finalState
        .view
        .map(syms2)
        .sum
    }
  }

  object Part1 extends Part {
    override type A = Pos3

    override def syms(pos: Pos3): Int = if (pos.z > 0) 1 else 0

    override def symKeep(pos: Pos3): Boolean = pos.z >= 0

    override val allOffsets: Seq[Pos3] = Pos3.allOffsets

    override def embedPos(pos: Pos): Pos3 = Pos3(pos.x, pos.y, 0)
  }

  object Part2 extends Part {
    override type A = Pos4

    override def syms(pos: Pos4): Int = (if (pos.z > 0) 1 else 0) + (if (pos.w > 0) 1 else 0)

    override def symKeep(pos: Pos4): Boolean = pos.z >= 0 && pos.w >= 0

    override val allOffsets: Seq[Pos4] = Pos4.allOffsets

    override def embedPos(pos: Pos): Pos4 = Pos4(pos.x, pos.y, 0, 0)
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countCubesBooted(parseGrid(input)))
    println(Part2.countCubesBooted(parseGrid(input)))
  }
}
