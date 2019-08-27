package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.pos.Pos

object Day3 {

  private val moveOffsets = Map(
    '^' -> Pos(0, 1),
    'v' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    '<' -> Pos(-1, 0),
  )

  def iteratePoss(moves: String): Iterator[Pos] = {
    moves.iterator.scanLeft(Pos.zero)(_ + moveOffsets(_))
  }

  def countAtLeastOne(moves: String): Int = {
    iteratePoss(moves).toSet.size
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countAtLeastOne(input))
  }
}
