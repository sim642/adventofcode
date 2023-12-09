package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IteratorImplicits.IndexIteratorOps
import eu.sim642.adventofcodelib.pos.Pos

object Day9 {

  case class Move(offset: Pos, n: Int)

  def moveTail(head: Pos, tail: Pos): Pos = {
    val d = head - tail
    if (d.x.abs <= 1 && d.y.abs <= 1) // touching
      tail
    else
      tail + Pos(d.x.sign, d.y.sign)
  }

  case class Rope(knots: List[Pos]) {

    def applyHeadOffset(offset: Pos): Rope = {

      def moveTails(knots: List[Pos]): List[Pos] = knots match {
        case Nil => throw new IllegalArgumentException("invalid empty rope")
        case List(head) => List(head)
        case head :: tail :: longTail =>
          val newTail = moveTail(head, tail)
          head :: moveTails(newTail :: longTail)
      }

      val newHead = knots.head + offset
      Rope(moveTails(newHead :: knots.tail))
    }

    def iterateMove(move: Move): Iterator[Rope] = {
      (1 to move.n).iterator.scanLeft(this)({ case (acc, _) => acc.applyHeadOffset(move.offset) }).tail
    }

    def tail: Pos = knots.last
  }

  def countTailPoss(moves: Seq[Move], length: Int): Int = {
    val initialRope = Rope(List.fill(length)(Pos.zero))
    // TODO: silly scanLeft+flatMap from 2016 day 1 again
    val ropeIt = moves.foldLeft((initialRope, Iterator.single(initialRope)))({ case ((accRope, accIt), move) =>
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countTailPoss(parseMoves(input), 2))
    println(countTailPoss(parseMoves(input), 10))
  }
}
