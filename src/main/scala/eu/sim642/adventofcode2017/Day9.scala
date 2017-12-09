package eu.sim642.adventofcode2017

object Day9 {

  def skipGarbage(chars: List[Char]): List[Char] = chars match {
    case '>' :: xs => xs
    case '!' :: _ :: xs => skipGarbage(xs)
    case _ :: xs => skipGarbage(xs)
  }

  def stripGarbage(chars: List[Char]): List[Char] = chars match {
    case Nil => Nil
    case '<' :: xs => stripGarbage(skipGarbage(xs))
    case x :: xs => x :: stripGarbage(xs)
  }

  def countGroups(chars: List[Char]): Int = {
    stripGarbage(chars).count(c => c == '{')
  }

  def countGroups(str: String): Int = countGroups(str.toList)

  def countScore(chars: List[Char]): Int = {
    val (1, score) = stripGarbage(chars).foldLeft((1, 0)) {
      case ((depth, score), x) =>
        x match {
          case '{' => (depth + 1, score + depth)
          case '}' => (depth - 1, score)
          case ',' => (depth, score)
        }
    }
    score
  }

  def countScore(str: String): Int = countScore(str.toList)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countScore(input))
  }
}
