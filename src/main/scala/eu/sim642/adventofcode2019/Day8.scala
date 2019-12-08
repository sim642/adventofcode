package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day8 {

  def fewestZeroLayer(layers: Seq[Grid[Int]]): Int = {
    // TODO: more optimal with groupBy?
    val layer = layers.minBy(_.countGrid(_ == 0))
    layer.countGrid(_ == 1) * layer.countGrid(_ == 2)
  }

  def parseLayers(input: String, width: Int = 25, height: Int = 6): Seq[Grid[Int]] = {
    input.toVector.map(_.asDigit).grouped(width * height).map(_.grouped(width).toVector).toSeq
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestZeroLayer(parseLayers(input)))
  }
}
