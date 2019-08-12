package eu.sim642.adventofcode2016

object Day21 {

  // similar to 2017 Day 1 CyclicSeq
  implicit class RotateString(s: String) {
    def rotate(n: Int): String = {
      import eu.sim642.adventofcode2016.Day15.IntModPos
      val realN = n %+ s.length
      val (init, tail) = s.splitAt(realN)
      tail + init
    }
  }

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
    case RotateSteps(x) =>
      s.rotate(x)
    case RotateLetter(x) =>
      val i = s.indexOf(x)
      val rotate = 1 + i + (if (i >= 4) 1 else 0)
      s.rotate(-rotate)
    case Reverse(x, y) =>
      val (lo, hi) = (x min y, (x max y) + 1)
      s.take(lo) + s.view.slice(lo, hi).reverse.mkString + s.drop(hi)
    case Move(x, y) =>
      val deleted = s.take(x) + s.drop(x + 1)
      val (init, tail) = deleted.splitAt(y)
      init + s(x) + tail
  }

  // RotateLetter(x) inverse:
  // x index before: i
  // x index after: j = (i + 1 + i + (if (i >= 4) 1 else 0)) % length
  // turns out to be invertible with length 8


  def applyOperations(s: String, operations: Seq[Operation]): String = {
    operations.foldLeft(s)(applyOperation)
  }

  def scramble(operations: Seq[Operation], s: String): String = applyOperations(s, operations)

  def unscramble(operations: Seq[Operation], s: String): String = {
    s.permutations.find(scramble(operations, _) == s).get
  }


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

  private val scrambleString = "abcdefgh"
  private val unscrambleString = "fbgdceah"

  def scramble(input: String, s: String = scrambleString): String = scramble(parseInput(input), s)

  def unscramble(input: String, s: String = unscrambleString): String = unscramble(parseInput(input), s)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(scramble(input))
    println(unscramble(input))

    // hgacdbef - not right
  }
}
