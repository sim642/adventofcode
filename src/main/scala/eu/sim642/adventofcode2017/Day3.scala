package eu.sim642.adventofcode2017

import scala.collection.{AbstractIterator, mutable}

object Day3 {

  def coord(square: Int): (Int, Int) = {
    val odds = Stream.from(1, 2)
    val boundSize = odds.find(x => x * x >= square).get
    val boundOffset = boundSize / 2
    val edgeSize = boundSize - 1
    val boundArea = boundSize * boundSize
    val innerArea = boundArea - 4 * edgeSize
    val innerOffset = boundOffset - 1

    val edgeSquare = square - innerArea - 1

    if (edgeSquare < edgeSize) // right
      (innerOffset + 1, -innerOffset + edgeSquare)
    else if (edgeSize <= edgeSquare && edgeSquare < 2 * edgeSize) // top
      (innerOffset - (edgeSquare - edgeSize), innerOffset + 1)
    else if (2 * edgeSize <= edgeSquare && edgeSquare < 3 * edgeSize) // left
      (-innerOffset - 1, innerOffset - (edgeSquare - 2 * edgeSize))
    else if (3 * edgeSize <= edgeSquare && edgeSquare < 4 * edgeSize) // bottom
      (-innerOffset + (edgeSquare - 3 * edgeSize), -innerOffset - 1)
    else
      throw new RuntimeException("impossible")
  }

  def distance(square: Int): Int = {
    val (x, y) = coord(square)
    x.abs + y.abs
  }

  class SumIterator extends AbstractIterator[Int] {

    implicit class CoordPair(p: (Int, Int)) {
      def +(q: (Int, Int)): (Int, Int) = (p._1 + q._1, p._2 + q._2)
    }

    private val sums = mutable.Map[(Int, Int), Int]()
    private var square = 1

    private val offsetCoords = Seq(
      (-1, 1), (0, 1), (1, 1),
      (-1, 0), (1, 0),
      (-1, -1), (0, -1), (1, -1)
    )

    override def hasNext: Boolean = true

    override def next(): Int = {
      val squareCoord = coord(square)
      if (square == 1) {
        sums += squareCoord -> 1
        square += 1
        1
      }
      else {
        val neighSums = for {
          offsetCoord <- offsetCoords
          coord = squareCoord + offsetCoord
          if sums.contains(coord)
        } yield sums(coord)

        val sum = neighSums.sum
        sums += squareCoord -> sum
        square += 1
        sum
      }
    }
  }

  def sumLarger(input: Int): Int = {
    val it = new SumIterator
    it.find(x => x > input).get
  }

  val input: Int = 289326

  def main(args: Array[String]): Unit = {
    println(distance(input))
    println(sumLarger(input))
  }
}
