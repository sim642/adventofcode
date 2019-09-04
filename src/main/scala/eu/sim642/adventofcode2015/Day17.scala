package eu.sim642.adventofcode2015

object Day17 {

  def countCombinations(sizes: List[Int], remaining: Int): Int = {
    if (remaining == 0)
      1
    else if (remaining < 0)
      0
    else {
      sizes match {
        case Nil => 0
        case hd :: tl =>
          countCombinations(tl, remaining) +
            countCombinations(tl, remaining - hd)
      }
    }
  }

  def countCombinations(input: String, remaining: Int = 150): Int = {
    val sizes = input.linesIterator.map(_.toInt).toList
    countCombinations(sizes, remaining)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countCombinations(input))
  }
}
