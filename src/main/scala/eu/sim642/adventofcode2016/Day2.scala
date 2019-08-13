package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2017.Day14.PosGrid
import eu.sim642.adventofcode2017.Day19.PosGrid2

object Day2 {

  type Move = Char

  private val moveOffsets: Map[Move, Pos] =
    Map(
      'U' -> Pos(0, -1),
      'D' -> Pos(0, 1),
      'L' -> Pos(-1, 0),
      'R' -> Pos(1, 0),
    )

  implicit class PosOfGrid[A](grid: Grid[A]) {
    def posOf(elem: A): Pos = {
      for {
        (row, y) <- grid.zipWithIndex
        (cell, x) <- row.zipWithIndex
        if cell == elem
      } return Pos(x, y)

      Pos(-1, -1)
    }
  }

  trait Part {
    val keypad: Grid[Option[Char]]

    def bathroomCode(allMoves: Seq[String]): String = {
      def move(pos: Pos, move: Move): Pos = {
        val newPos = pos + moveOffsets(move)
        if (keypad.containsPos(newPos) && keypad(newPos).isDefined)
          newPos
        else
          pos
      }

      def moves(pos: Pos, moves: Seq[Move]): Pos = moves.foldLeft(pos)(move)

      val initialPos = keypad.posOf(Some('5'))
      allMoves.scanLeft(initialPos)(moves(_, _)).tail.map(keypad(_).get).mkString("")
    }

    def bathroomCode(input: String): String = bathroomCode(input.lines.toSeq)
  }

  object Part1 extends Part {
    override val keypad: Grid[Option[Char]] =
      Vector(
        Vector(Some('1'), Some('2'), Some('3')),
        Vector(Some('4'), Some('5'), Some('6')),
        Vector(Some('7'), Some('8'), Some('9')),
      )
  }

  object Part2 extends Part {
    override val keypad: Grid[Option[Char]] =
      Vector(
        Vector(     None,      None, Some('1'),      None,      None),
        Vector(     None, Some('2'), Some('3'), Some('4'),      None),
        Vector(Some('5'), Some('6'), Some('7'), Some('8'), Some('9')),
        Vector(     None, Some('A'), Some('B'), Some('C'),      None),
        Vector(None,           None, Some('D'),      None,      None),
      )
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.bathroomCode(input))
    println(Part2.bathroomCode(input))
  }
}
