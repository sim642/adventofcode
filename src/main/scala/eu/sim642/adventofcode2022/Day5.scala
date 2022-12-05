package eu.sim642.adventofcode2022

object Day5 {

  type Crate = Char
  type Stack = Seq[Crate]

  case class Move(count: Int, from: Int, to: Int)

  def applyMove(crates: Seq[Stack], move: Move): Seq[Stack] = {
    val Move(count, from, to) = move
    val (moveCrates, newFromCrates) = crates(from).splitAt(count)
    crates
      .updated(from, newFromCrates)
      .updated(to, moveCrates.reverse ++ crates(to))
  }

  def applyMoves(crates: Seq[Stack], moves: Seq[Move]): Seq[Stack] = {
    moves.foldLeft(crates)(applyMove)
  }

  def topMessage(crates: Seq[Stack], moves: Seq[Move]): String = {
    applyMoves(crates, moves)
      .map(_.head)
      .mkString
  }


  def parseCrates(s: String): Seq[Stack] = {
    val lines = s.linesIterator.toSeq
    val maxLineLength = lines.view.map(_.length).max
    val paddedLines = lines.map(_.padTo(maxLineLength, ' '))
    val cols = paddedLines.reverse.transpose
    (1 until cols.size by 4)
      .map(i => cols(i).tail.filter(_ != ' ').reverse)
  }

  private val moveRegex = """move (\d+) from (\d+) to (\d+)""".r

  def parseMove(s: String): Move = s match {
    case moveRegex(count, from, to) =>
      Move(count.toInt, from.toInt - 1, to.toInt - 1)
  }

  def parseMoves(s: String): Seq[Move] = s.linesIterator.map(parseMove).toSeq

  def parseInput(input: String): (Seq[Stack], Seq[Move]) = {
    val Array(crateStr, moveStr) = input.split("\n\n", 2)
    (parseCrates(crateStr), parseMoves(moveStr))
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(topMessage.tupled(parseInput(input)))
  }
}
