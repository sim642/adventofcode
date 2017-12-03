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
    if (square == 1)
      Pos(0, 0)
    else {
      val odds = Stream.from(1, 2)
      val boundSize = odds.find(x => x * x >= square).get
      val boundOffset = boundSize / 2
      val edgeSize = boundSize - 1
      val boundArea = boundSize * boundSize
      val innerArea = boundArea - 4 * edgeSize
      val innerOffset = boundOffset - 1

      val edgeSquare = square - innerArea - 1

      if ((0 until edgeSize).contains(edgeSquare)) // right
        Pos(innerOffset + 1, -innerOffset + edgeSquare)
      else if ((edgeSize until 2 * edgeSize).contains(edgeSquare)) // top
        Pos(innerOffset - (edgeSquare - edgeSize), innerOffset + 1)
      else if ((2 * edgeSize until 3 * edgeSize).contains(edgeSquare)) // left
        Pos(-innerOffset - 1, innerOffset - (edgeSquare - 2 * edgeSize))
      else if ((3 * edgeSize until 4 * edgeSize).contains(edgeSquare)) // bottom
        Pos(-innerOffset + (edgeSquare - 3 * edgeSize), -innerOffset - 1)
      else
        throw new RuntimeException("impossible")
    }
  }

  def steps(square: Int): Int = {
    squarePos(square) manhattanDistance Pos(0, 0)
  }

  class SpiralSumIterator extends AbstractIterator[Int] {

    private val sums = mutable.Map[Pos, Int]()
    private var square = 1

    override def hasNext: Boolean = true

    override def next(): Int = {
      val pos = squarePos(square)
      val sum = if (square == 1)
        1
      else {
        val adjacentSums = for {
          offset <- Pos.allOffsets
          offsetPos = pos + offset
        } yield sums.getOrElse(offsetPos, 0)
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

  val input: Int = 289326

  def main(args: Array[String]): Unit = {
    println(steps(input))
    println(spiralSumLarger(input))
  }
}
