package eu.sim642.adventofcode2017

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
    println((x, y))
    x.abs + y.abs
  }

  val input: Int = 289326

  def main(args: Array[String]): Unit = {
    println(distance(input))
  }
}
