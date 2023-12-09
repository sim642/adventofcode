package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

import scala.collection.mutable

object Day11 {

  def hundredsDigit(n: Int): Int = n / 100 % 10

  def powerLevel(serialNumber: Int)(pos: Pos): Int = {
    val rackID = pos.x + 10
    hundredsDigit((rackID * pos.y + serialNumber) * rackID) - 5
  }

  trait SumGrid {
    def sumBox(box: Box): Int
  }

  class NaiveSumGrid(f: Pos => Int) extends SumGrid {
    override def sumBox(box: Box): Int = {
      box.iterator.map(f).sum
    }
  }

  class PartialSumGrid(f: Pos => Int, box: Box) extends SumGrid {
    val Box(min, max) = box
    val partialSums: mutable.Map[Pos, Int] = mutable.Map.empty.withDefaultValue(0)

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        val sum = partialSums(pos + Pos(0, -1)) + partialSums(pos + Pos(-1, 0)) - partialSums(pos + Pos(-1, -1)) + f(pos)
        partialSums.put(pos, sum)
      }
    }

    override def sumBox(box: Box): Int = {
      val Box(topLeft, bottomRight) = box
      val bottomLeft1 = Pos(topLeft.x - 1, bottomRight.y)
      val topRight1 = Pos(bottomRight.x, topLeft.y - 1)
      val topLeft1 = Pos(topLeft.x - 1, topLeft.y - 1)
      partialSums(bottomRight) - partialSums(bottomLeft1) - partialSums(topRight1) + partialSums(topLeft1)
    }
  }

  private val totalBox = Box(Pos(1, 1), Pos(300, 300))

  def getSumGrid(serialNumber: Int): SumGrid = {
    new PartialSumGrid(powerLevel(serialNumber), totalBox)
  }

  implicit class SlidingBox(box: Box) {
    def sliding(size: Pos): Iterator[Box] = {
      val sizeOffset = size - Pos(1, 1)
      val minBox = Box(box.min, box.max - sizeOffset)
      minBox.iterator.map(min => Box(min, min + sizeOffset))
    }
  }

  def largestPowerLevelSquare(serialNumber: Int): Pos = {
    val sumGrid = getSumGrid(serialNumber)
    totalBox.sliding(Pos(3, 3)).maxBy(sumGrid.sumBox).min
  }

  def largestPowerLevelSquareString(serialNumber: Int): String = {
    val pos = largestPowerLevelSquare(serialNumber)
    s"${pos.x},${pos.y}"
  }

  def largestPowerLevelSquare2(serialNumber: Int): (Pos, Int) = {
    val sumGrid = getSumGrid(serialNumber)
    val maxSumBox = (for {
      size <- (1 to 300).iterator
      box <- totalBox.sliding(Pos(size, size))
    } yield box).maxBy(sumGrid.sumBox)
    (maxSumBox.min, maxSumBox.max.x - maxSumBox.min.x + 1) // recompute size assuming square
  }

  def largestPowerLevelSquareString2(serialNumber: Int): String = {
    val (pos, size) = largestPowerLevelSquare2(serialNumber)
    s"${pos.x},${pos.y},$size"
  }


  //lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim
  val input = 7403

  def main(args: Array[String]): Unit = {
    println(largestPowerLevelSquareString(input))
    println(largestPowerLevelSquareString2(input))
  }
}
