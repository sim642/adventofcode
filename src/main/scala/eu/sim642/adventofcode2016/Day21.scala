package eu.sim642.adventofcode2016

object Day21 {

  sealed trait Operation
  case class SwapPosition(x: Int, y: Int) extends Operation
  case class SwapLetter(x: Char, y: Char) extends Operation
  case class RotateSteps(x: Int) extends Operation
  case class RotateLetter(x: Char) extends Operation
  case class Reverse(x: Int, y: Int) extends Operation
  case class Move(x: Int, y: Int) extends Operation

  def applyOperation(s: String, operation: Operation): String = operation match {
    case SwapPosition(x, y) =>
      s.updated(x, s(y)).updated(y, s(x))
    case SwapLetter(x, y) =>
      s.map({
        case c if c == x => y
        case c if c == y => x
        case c => c
      })
    case RotateSteps(x) if x >= 0 =>
      s.drop(x) + s.take(x)
    case RotateSteps(x) if x < 0 =>
      s.takeRight(-x) + s.dropRight(-x)
    case RotateLetter(x) =>
      val i = s.indexOf(x)
      val rotate = (1 + i + (if (i >= 4) 1 else 0)) % s.length
      s.takeRight(rotate) + s.dropRight(rotate)
    case Reverse(x, y) =>
      val lo = x min y
      val hi = (x max y) + 1
      s.take(lo) + s.view(lo, hi).reverse.mkString + s.drop(hi)
    case Move(x, y) =>
      val deleted = s.take(x) + s.drop(x + 1)
      deleted.take(y) + s(x) + deleted.drop(y)
  }

  def applyOperations(s: String, operations: Seq[Operation]): String = {
    operations.foldLeft(s)(applyOperation)
  }

  private val scrambleString = "abcdefgh"

  def scramble(operations: Seq[Operation], s: String): String = applyOperations(s, operations)


  private val swapPositionRegex = """swap position (\d+) with position (\d+)""".r
  private val swapLetterRegex = """swap letter (.) with letter (.)""".r
  private val rotateStepsRegex = """rotate (left|right) (\d+) steps?""".r
  private val rotateLetterRegex = """rotate based on position of letter (.)""".r
  private val reverseRegex = """reverse positions (\d+) through (\d+)""".r
  private val moveRegex = """move position (\d+) to position (\d+)""".r

  def parseOperation(s: String): Operation = s match {
    case swapPositionRegex(x, y) => SwapPosition(x.toInt, y.toInt)
    case swapLetterRegex(x, y) => SwapLetter(x.head, y.head)
    case rotateStepsRegex("left", x) => RotateSteps(x.toInt)
    case rotateStepsRegex("right", x) => RotateSteps(-x.toInt)
    case rotateLetterRegex(x) => RotateLetter(x.head)
    case reverseRegex(x, y) => Reverse(x.toInt, y.toInt)
    case moveRegex(x, y) => Move(x.toInt, y.toInt)
  }

  def parseInput(input: String): Seq[Operation] = input.lines.map(parseOperation).toSeq

  def scramble(input: String, s: String = scrambleString): String = scramble(parseInput(input), s)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    print(scramble(input))
  }
}
