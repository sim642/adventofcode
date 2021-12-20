package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

object Day20 {

  case class Image(grid: Grid[Boolean], outer: Boolean)
  case class Input(algorithm: Vector[Boolean], image: Image)

  private val offsets = for {
    dy <- -1 to 1
    dx <- -1 to 1
  } yield Pos(dx, dy) // includes 0,0

  def enhance(algorithm: Vector[Boolean], image: Image): Image = {
    val Image(grid, outer) = image
    val extendedGrid = Vector.fill(grid(0).size + 2)(outer) +: grid.map(outer +: _ :+ outer) :+ Vector.fill(grid(0).size + 2)(outer)
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
    val enhancedOuter = outer match {
      case false if algorithm(0) => true
      case true if !algorithm(255) => false
      case _ => outer
    }
    Image(enhancedGrid, enhancedOuter)
  }

  def countEnhanced(input: Input): Int = {
    val Input(algorithm, image) = input
    val image2 = enhance(algorithm, enhance(algorithm, image))
    image2.grid.countGrid(identity)
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
  }
}
