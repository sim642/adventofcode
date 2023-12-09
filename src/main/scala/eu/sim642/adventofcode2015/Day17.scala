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

  def countCombinations(input: String, remaining: Int = 150): Int = countCombinations(parseSizes(input), remaining)

  def countMinCombinations(sizes: List[Int], remaining: Int): Int = {
    // TODO: prune by length exceeding found length
    def helper(sizes: List[Int], length: Int, remaining: Int): Option[(Int, Int)] = {
      if (remaining == 0)
        Some((length, 1))
      else if (remaining < 0)
        None
      else {
        sizes match {
          case Nil => None
          case hd :: tl =>
            // TODO: simplify the match and if
            (helper(tl, length, remaining), helper(tl, length + 1, remaining - hd)) match {
              case (None, None) => None
              case (left, None) => left
              case (None, right) => right
              case (left@Some((leftLength, leftCount)), right@Some((rightLength, rightCount))) =>
                if (leftLength < rightLength)
                  left
                else if (leftLength > rightLength)
                  right
                else
                  Some((leftLength, leftCount + rightCount))
            }
        }
      }
    }

    helper(sizes, 0, remaining).map(_._2).getOrElse(0)
  }

  def countMinCombinations(input: String, remaining: Int = 150): Int = countMinCombinations(parseSizes(input), remaining)


  def parseSizes(input: String): List[Int] = input.linesIterator.map(_.toInt).toList

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countCombinations(input))
    println(countMinCombinations(input))
  }
}
