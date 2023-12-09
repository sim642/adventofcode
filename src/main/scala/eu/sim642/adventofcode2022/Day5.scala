package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.GridImplicits._

object Day5 {

  type Crate = Char
  type Stack = Seq[Crate]

  case class Move(count: Int, from: Int, to: Int)

  case class Input(crates: Seq[Stack], moves: Seq[Move])

  trait Part {
    def mapMoveCrates(moveCrates: Stack): Stack

    def applyMove(crates: Seq[Stack], move: Move): Seq[Stack] = {
      val Move(count, from, to) = move
      val (moveCrates, newFromCrates) = crates(from).splitAt(count)
      crates
        .updated(from, newFromCrates)
        .updated(to, mapMoveCrates(moveCrates) ++ crates(to))
    }

    def applyMoves(crates: Seq[Stack], moves: Seq[Move]): Seq[Stack] = {
      moves.foldLeft(crates)(applyMove)
    }

    def topMessage(input: Input): String = {
      applyMoves(input.crates, input.moves)
        .map(_.head)
        .mkString
    }
  }

  object Part1 extends Part {
    override def mapMoveCrates(moveCrates: Stack): Stack = moveCrates.reverse
  }

  object Part2 extends Part {
    override def mapMoveCrates(moveCrates: Stack): Stack = moveCrates
  }


  def parseCrates(s: String): Seq[Stack] = {
    val rows = s.linesIterator.map(_.toVector).toVector.padGrid(' ') // pad lines just for example input in tests
    val cols = rows.reverse.transpose
    (1 until cols.size by 4)
      .map(cols(_).tail.filter(_ != ' ').reverse)
  }

  private val moveRegex = """move (\d+) from (\d+) to (\d+)""".r

  def parseMove(s: String): Move = s match {
    case moveRegex(count, from, to) =>
      Move(count.toInt, from.toInt - 1, to.toInt - 1)
  }

  def parseMoves(s: String): Seq[Move] = s.linesIterator.map(parseMove).toSeq

  def parseInput(input: String): Input = {
    val Array(crateStr, moveStr) = input.split("\n\n", 2)
    Input(parseCrates(crateStr), parseMoves(moveStr))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(Part1.topMessage(parseInput(input)))
    println(Part2.topMessage(parseInput(input)))
  }
}
