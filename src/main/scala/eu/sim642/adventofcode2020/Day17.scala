package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.pos.{Pos, Pos3, Pos4, PosOps}

import scala.language.implicitConversions

object Day17 {

  implicit def boolean2Int(b: Boolean): Int = if (b) 1 else 0

  sealed trait Part {
    type A <: PosOps[A]

    val zero: A
    val allOffsets: Seq[A]

    def relativeSymmetryDimensions(pos1: A, pos2: A): Int
    private def relativeSymmetries(pos1: A, pos2: A): Int = 1 << relativeSymmetryDimensions(pos1, pos2)

    def neighborPredicate(pos: A): Boolean

    private def neighbors(pos: A): Iterator[A] = allOffsets.iterator.map(pos + _)

    def step(state: Set[A]): Set[A] = {
      println(countSymmetric(state))
      state.iterator
        .flatMap(pos =>
          neighbors(pos)
            .filter(neighborPredicate)
            .flatMap(neigh =>
              Iterator.fill(relativeSymmetries(pos, neigh))(neigh)
            )
        )
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

    override def relativeSymmetryDimensions(pos1: Pos3, pos2: Pos3): Int = {
      (pos1.z > 0 && pos2.z == 0).toInt
    }

    override def neighborPredicate(pos: Pos3): Boolean = pos.z >= 0

    override def embedPos(pos: Pos): Pos3 = Pos3(pos.x, pos.y, 0)
  }

  object Part2 extends Part {
    override type A = Pos4

    override val zero: Pos4 = Pos4.zero
    override val allOffsets: Seq[Pos4] = Pos4.allOffsets

    override def relativeSymmetryDimensions(pos1: Pos4, pos2: Pos4): Int = {
      (pos1.z > 0 && pos2.z == 0).toInt + (pos1.w > 0 && pos2.w == 0).toInt
    }

    override def neighborPredicate(pos: Pos4): Boolean = pos.z >= 0 && pos.w >= 0

    override def embedPos(pos: Pos): Pos4 = Pos4(pos.x, pos.y, 0, 0)
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countCubesBooted(parseGrid(input)))
    println(Part2.countCubesBooted(parseGrid(input)))
  }
}
