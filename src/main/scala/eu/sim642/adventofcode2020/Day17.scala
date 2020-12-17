package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.box.{Box3, Box4}
import eu.sim642.adventofcodelib.pos.{Pos3, Pos4}

object Day17 {

  // TODO: reduce duplication

  object Part1 {

    type State = Vector[Grid[Boolean]] // TODO: Grid3?

    val allOffsets: Seq[Pos3] =
      for {
        z <- -1 to 1
        y <- -1 to 1
        x <- -1 to 1
        pos = Pos3(x, y, z)
        if pos != Pos3.zero
      } yield pos


    def printState(state: State): Unit = {
      for (plane <- state) {
        for (row <- plane) {
          for (cell <- row)
            print(if (cell) '#' else '.')
          println()
        }
        println()
        println()
      }
    }

    def step(state: State): State = {
      val max = Pos3(state.head.head.size, state.head.size, state.size)
      val paddedState = {
        val emptyPlane = Vector.fill(max.y + 2, max.x + 2)(false)
        val emptyRow = Vector.fill(max.x + 2)(false)
        emptyPlane +: state.map(plane => emptyRow +: plane.map(row => false +: row :+ false) :+ emptyRow) :+ emptyPlane
      }
      //val paddedBox = Box3(Pos3.zero, max + Pos3(1, 1, 1))
      val paddedBox = Box3(Pos3.zero, max)
      //printState(state)
      //println("---")
      //printState(paddedState)
      for ((plane, z) <- paddedState.zipWithIndex)
        yield for ((row, y) <- plane.zipWithIndex)
          yield for ((cell, x) <- row.zipWithIndex)
            yield {
              val pos = Pos3(x, y, z)
              //println(pos)
              val neighbors = allOffsets.map(pos + _).collect({
                case pos@Pos3(x, y, z) if paddedBox.contains(pos) =>
                  //println(s"  $pos")
                  paddedState(z)(y)(x)
              })
              val active = neighbors.count(identity)
              cell match {
                case true if active == 2 || active == 3 => true
                case true => false
                case false if active == 3 => true
                case false => false
              }
            }
    }

    def countCubesBooted(grid: Grid[Boolean], steps: Int = 6): Int = {
      val finalState = Iterator.iterate(Vector(grid))(step)(steps)
      finalState
        .view
        .map(_.countGrid(identity))
        .sum
    }
  }

  object Part2 {

    type State = Grid[Grid[Boolean]] // TODO: Grid4?

    val allOffsets: Seq[Pos4] =
      for {
        w <- -1 to 1
        z <- -1 to 1
        y <- -1 to 1
        x <- -1 to 1
        pos = Pos4(x, y, z, w)
        if pos != Pos4.zero
      } yield pos


    /*def printState(state: State): Unit = {
      for (plane <- state) {
        for (row <- plane) {
          for (cell <- row)
            print(if (cell) '#' else '.')
          println()
        }
        println()
        println()
      }
    }*/

    def step(state: State): State = {
      val max = Pos4(state.head.head.head.size, state.head.head.size, state.head.size, state.size)
      val paddedState: State = {
        val emptyCube = Vector.fill(max.z + 2, max.y + 2, max.x + 2)(false)
        val emptyPlane = Vector.fill(max.y + 2, max.x + 2)(false)
        val emptyRow = Vector.fill(max.x + 2)(false)
        emptyCube +: state.map(cube =>
          emptyPlane +: cube.map(plane =>
            emptyRow +: plane.map(row =>
              false +: row :+ false
            ) :+ emptyRow
          ) :+ emptyPlane
        ) :+ emptyCube
      }
      //val paddedBox = Box3(Pos3.zero, max + Pos3(1, 1, 1))
      val paddedBox = Box4(Pos4.zero, max)
      //printState(state)
      //println("---")
      //printState(paddedState)
      for ((cube, w) <- paddedState.zipWithIndex)
        yield for ((plane, z) <- cube.zipWithIndex)
          yield for ((row, y) <- plane.zipWithIndex)
            yield for ((cell, x) <- row.zipWithIndex)
              yield {
                val pos = Pos4(x, y, z, w)
                //println(pos)
                val neighbors = allOffsets.map(pos + _).collect({
                  case pos@Pos4(x, y, z, w) if paddedBox.contains(pos) =>
                    //println(s"  $pos")
                    paddedState(w)(z)(y)(x)
                })
                val active = neighbors.count(identity)
                cell match {
                  case true if active == 2 || active == 3 => true
                  case true => false
                  case false if active == 3 => true
                  case false => false
                }
              }
    }

    def countCubesBooted(grid: Grid[Boolean], steps: Int = 6): Int = {
      val finalState = Iterator.iterate(Vector(Vector(grid)))(step)(steps)
      finalState
        .mapGrid(_.countGrid(identity))
        .sumGrid
    }
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countCubesBooted(parseGrid(input)))
    println(Part2.countCubesBooted(parseGrid(input)))
  }
}
