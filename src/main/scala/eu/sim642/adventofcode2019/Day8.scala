package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day8 {

  def fewestZeroLayer(layers: Seq[Grid[Int]]): Int = {
    // TODO: more optimal with groupBy?
    val layer = layers.minBy(_.countGrid(_ == 0))
    layer.countGrid(_ == 1) * layer.countGrid(_ == 2)
  }

  def stackLayers(layers: Seq[Grid[Int]]): Grid[Int] = {
    def stack2(top: Grid[Int], bottom: Grid[Int]): Grid[Int] = {
      // TODO: zip for Grid?
      (for ((topRow, bottomRow) <- top lazyZip bottom)
        yield (for ((topCell, bottomCell) <- topRow lazyZip bottomRow)
          yield {
            (topCell, bottomCell) match {
              case (2, bottomCell) => bottomCell // top transparent
              case (topCell, _) => topCell
            }
          })
        )
    }

    layers.reduce(stack2)
  }

  // copied & modified from 2018 Day 18
  def printGrid(grid: Grid[Int]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell match {
          case 0 => '#' // black
          case 1 => '.' // white
        })
      println()
    }
  }

  def parseLayers(input: String, width: Int = 25, height: Int = 6): Seq[Grid[Int]] = {
    input.toVector.map(_.asDigit).grouped(width * height).map(_.grouped(width).toVector).toSeq
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestZeroLayer(parseLayers(input)))
    printGrid(stackLayers(parseLayers(input))) // EHRUE
  }
}
