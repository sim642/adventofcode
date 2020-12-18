package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.pos.{Pos, Pos3, Pos4, PosOps}

object Day17 {

  def relativeSymmetry(value1: Int, value2: Int): Int = {
    if (value2 < 0)
      0
    else if (value1 > 0 && value2 == 0)
      2
    else
      1
  }

  // TODO: add back non-symmetric solution? just make relativeSymmetries 1?

  sealed trait Part {
    type A <: PosOps[A]

    val zero: A
    val allOffsets: Seq[A]

    def relativeSymmetries(pos1: A, pos2: A): Int

    private def neighbors(pos: A): Iterator[A] = allOffsets.iterator.map(pos + _)

    def step(state: Set[A]): Set[A] = {
      state.iterator
        .flatMap(pos =>
          neighbors(pos)
            .map(neigh =>
              neigh -> relativeSymmetries(pos, neigh)
            )
        )
        .groupMapReduce(_._1)(_._2)(_ + _)
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

    private def symmetries(pos: A): Int = relativeSymmetries(pos, zero)

    private def countSymmetric(state: Set[A]) = {
      state
        .view
        .map(symmetries)
        .sum
    }

    def countCubesBooted(grid: Grid[Boolean], steps: Int = 6): Int = {
      val initialState = fromGrid(grid)
      val finalState = Iterator.iterate(initialState)(step)(steps)
      countSymmetric(finalState)
    }
  }

  object Part1 extends Part {
    override type A = Pos3

    override val zero: Pos3 = Pos3.zero
    override val allOffsets: Seq[Pos3] = Pos3.allOffsets

    override def relativeSymmetries(pos1: Pos3, pos2: Pos3): Int = {
      relativeSymmetry(pos1.z, pos2.z)
    }

    override def embedPos(pos: Pos): Pos3 = Pos3(pos.x, pos.y, 0)
  }

  object Part2 extends Part {
    override type A = Pos4

    override val zero: Pos4 = Pos4.zero
    override val allOffsets: Seq[Pos4] = Pos4.allOffsets

    // TODO: also use symmetry around w=z axis

    override def relativeSymmetries(pos1: Pos4, pos2: Pos4): Int = {
      relativeSymmetry(pos1.z, pos2.z) * relativeSymmetry(pos1.w, pos2.w)
    }

    override def embedPos(pos: Pos): Pos4 = Pos4(pos.x, pos.y, 0, 0)
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countCubesBooted(parseGrid(input)))
    println(Part2.countCubesBooted(parseGrid(input)))
  }
}
