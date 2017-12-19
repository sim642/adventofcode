package eu.sim642.adventofcode2017

import Day3.Pos
import Day14.PosGrid

object Day19 {

  type Grid[A] = Vector[Vector[A]]

  implicit class Pos2(pos: Pos) {
    def unary_-(): Pos = Pos(-pos.x, -pos.y)
  }

  implicit class PosGrid2[A](grid: Grid[A]) {
    def containsPos(pos: Pos): Boolean = grid.indices.contains(pos.y) && grid(pos.y).indices.contains(pos.x)
  }

  def pathLetters(grid: Grid[Char]): String = {

    def helper(pos: Pos, prevOffset: Pos, letters: String = ""): String = grid(pos) match {
      case ' ' =>
        letters
      case '|' =>
        helper(pos + prevOffset, prevOffset, letters)
      case '-' =>
        helper(pos + prevOffset, prevOffset, letters)
      case '+' =>
        val offsets = Pos.axisOffsets.toSet - (-prevOffset)
        val offset = offsets.find(offset => grid.containsPos(pos + offset) && grid(pos + offset) != ' ').get
        helper(pos + offset, offset, letters)
      case x if x.isLetter =>
        helper(pos + prevOffset, prevOffset, letters + x)
    }

    val startX = grid(0).indexOf('|')
    helper(Pos(startX, 0), Pos(0, 1))
  }

  def pathLetters(input: String): String = pathLetters(input.lines.map(_.toVector).toVector)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(pathLetters(input))
  }
}
