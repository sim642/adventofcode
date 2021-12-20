package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day20 {

  case class Image(grid: Grid[Boolean], outer: Boolean)
  case class Input(algorithm: Vector[Boolean], image: Image)

  private val offsets = for {
    dy <- -1 to 1
    dx <- -1 to 1
  } yield Pos(dx, dy) // includes 0,0

  def enhance(algorithm: Vector[Boolean], image: Image): Image = {
    val Image(grid, outer) = image
    val extendRow = Vector.fill(grid(0).size + 2)(outer)
    val extendedGrid = extendRow +: grid.map(outer +: _ :+ outer) :+ extendRow
    val enhancedGrid =
      for ((row, y) <- extendedGrid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            val neighbors = offsets.map(pos + _)
            val values = neighbors.map(neighbor => {
              if (extendedGrid.containsPos(neighbor))
                extendedGrid(neighbor)
              else
                outer
            })
            val i = Day3.binary2int(values)
            algorithm(i)
          }
    val enhancedOuter = algorithm(if (outer) 511 else 0)
    Image(enhancedGrid, enhancedOuter)
  }

  def countEnhanced(input: Input, after: Int = 2): Int = {
    val Input(algorithm, image) = input
    val finalImage = Iterator.iterate(image)(enhance(algorithm, _))(after)
    finalImage.grid.countGrid(identity)
  }


  def printImage(image: Image): Unit = {
    val Image(grid, outer) = image
    for (row <- grid) {
      for (cell <- row)
        print(if (cell) '#' else '.')
      println()
    }
    println(s"outer: $outer\n")
  }


  def parseImage(s: String): Image = {
    Image(s.linesIterator.map(_.map(_ == '#').toVector).toVector, false)
  }

  def parseInput(input: String): Input = {
    val Seq(algorithmStr, imageStr) = input.split("\n\n").toSeq
    val algorithm = algorithmStr.map(_ == '#').toVector
    val image = parseImage(imageStr)
    Input(algorithm, image)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countEnhanced(parseInput(input)))
    println(countEnhanced(parseInput(input), 50))

    // part 2: 20511 - too high (255 in blink instead of 511)
  }
}
