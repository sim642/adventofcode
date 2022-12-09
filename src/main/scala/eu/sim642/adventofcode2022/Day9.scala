package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IteratorImplicits.IndexIteratorOps
import eu.sim642.adventofcodelib.pos.Pos

object Day9 {

  case class Move(offset: Pos, n: Int)

  case class Rope(head: Pos, tail: Pos) {

    // TODO: copied
    private def clamp(min: Int, max: Int)(value: Int): Int = {
      if (value < min)
        min
      else if (value > max)
        max
      else value
    }

    private def moveTail: Rope = {
      val d = head - tail
      if (d.x.abs <= 1 && d.y.abs <= 1)
        this
      else if (d.x == 0 && d.y.abs == 2 || d.y == 0 && d.x.abs == 2)
        copy(tail = tail + Pos(d.x / 2, d.y / 2))
      else if (d.x.abs == 1 && d.y.abs == 2 || d.y.abs == 1 && d.x.abs == 2)
        copy(tail = tail + Pos(clamp(-1, 1)(d.x), (clamp(-1, 1)(d.y))))
      else
        throw new IllegalStateException("invalid tail position")
    }

    def applyHeadOffset(offset: Pos): Rope = {
      copy(head = head + offset).moveTail
    }

    def iterateMove(move: Move): Iterator[Rope] = {
      (1 to move.n).iterator.scanLeft(this)({ case (acc, _) => acc.applyHeadOffset(move.offset) }).tail
    }
  }

  def countTailPoss(moves: Seq[Move]): Int = {
    val initialRope = Rope(Pos.zero, Pos.zero)
    // TODO: silly scanLeft+flatMap from 2016 day 9 again
    val ropeIt = moves.foldLeft[(Rope, Iterator[Rope])]((initialRope, Iterator.single(initialRope)))({ case ((accRope: Rope, accIt: Iterator[Rope]), move: Move) =>
      (accRope.iterateMove(move).last, accIt ++ accRope.iterateMove(move))
    })._2

    ropeIt.map(_.tail).toSet.size
  }


  private val moveOffsets = Map(
    'R' -> Pos(1, 0),
    'L' -> Pos(-1, 0),
    'U' -> Pos(0, 1),
    'D' -> Pos(0, -1),
  )

  def parseMove(s: String): Move = s match {
    case s"$dir $n" => Move(moveOffsets(dir.charAt(0)), n.toInt)
  }

  def parseMoves(input: String): Seq[Move] = input.linesIterator.map(parseMove).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countTailPoss(parseMoves(input)))
  }
}
