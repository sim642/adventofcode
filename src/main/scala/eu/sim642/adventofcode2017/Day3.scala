package eu.sim642.adventofcode2017

import scala.collection.{AbstractIterator, mutable}

object Day3 {

  case class Pos(x: Int, y: Int) {
    def manhattanDistance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
  }

  object Pos {
    val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
    val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
    val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
  }

  def squarePos(square: Int): Pos = {
    def nextOddSqrt(n: Int): Int = {
      /*val odds = Stream.from(1, 2)
      odds.find(x => x * x >= n).get*/
      val nextSqrt = Math.sqrt(n).ceil.toInt
      nextSqrt + (if (nextSqrt % 2 == 0) 1 else 0)
    }

    if (square == 1)
      Pos(0, 0)
    else {
      val boundSize = nextOddSqrt(square)

      val innerSize = boundSize - 2
      val innerOffset = innerSize / 2
      val innerArea = innerSize * innerSize

      val edgeSize = boundSize - 1
      val edgeSquare = square - innerArea - 1
      val edgeOffset = edgeSquare % edgeSize

      edgeSquare / edgeSize match {
        case 0 => Pos(innerOffset + 1, -innerOffset + edgeOffset) // right
        case 1 => Pos(innerOffset - edgeOffset, innerOffset + 1) // top
        case 2 => Pos(-innerOffset - 1, innerOffset - edgeOffset) // left
        case 3 => Pos(-innerOffset + edgeOffset, -innerOffset - 1) // bottom
      }
    }
  }

  def steps(square: Int): Int = {
    squarePos(square) manhattanDistance Pos(0, 0)
  }

  /**
    * https://oeis.org/A141481
    */
  class SpiralSumIterator extends AbstractIterator[Int] {

    private val sums = mutable.Map[Pos, Int]()
    private var square = 1

    override def hasNext: Boolean = true

    override def next(): Int = {
      val pos = squarePos(square)
      val sum = if (square == 1)
        1
      else {
        val adjacentSums: Seq[Int] = Pos.allOffsets.flatMap(offset => sums.get(pos + offset))
        adjacentSums.sum
      }

      sums += pos -> sum
      square += 1
      sum
    }
  }

  def spiralSumLarger(input: Int): Int = {
    val it = new SpiralSumIterator
    it.find(x => x > input).get
  }

  lazy val input: Int = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim.toInt

  def main(args: Array[String]): Unit = {
    println(steps(input))
    println(spiralSumLarger(input))
  }
}
