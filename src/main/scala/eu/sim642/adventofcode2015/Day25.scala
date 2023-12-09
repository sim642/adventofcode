package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._

object Day25 {

  def posIndex(pos: Pos): Int = {
    val diagonal = pos.x + pos.y - 2
    val triangleSum = (1 to diagonal).sum
    val diagonalOffset = pos.x
    triangleSum + diagonalOffset
  }

  private val initialCode = 20151125
  private val base = 252533L // Long, so modPow squaring doesn't overflow
  private val modulo = 33554393

  def getCode(pos: Pos): Long = {
    val power = posIndex(pos) - 1
    (initialCode * base.modPow(power, modulo)) % modulo
  }

  def getCode(input: String): Long = getCode(parseInput(input))


  private val inputRegex = """To continue, please consult the code grid in the manual\.  Enter the code at row (\d+), column (\d+)\.""".stripMargin.r

  def parseInput(input: String): Pos = input match {
    case inputRegex(row, col) => Pos(col.toInt, row.toInt)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(getCode(input))
  }
}
