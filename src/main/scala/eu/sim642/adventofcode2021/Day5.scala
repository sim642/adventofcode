package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.collection.immutable.ArraySeq

object Day5 {

  type Line = (Pos, Pos)

  extension (start: Int) {
    def directedTo(end: Int): Range.Inclusive = {
      Range.inclusive(start, end, if (start > end) -1 else 1)
    }
  }

  sealed trait Solution {
    def countOverlaps(lines: Seq[Line], diagonal: Boolean = false): Int
  }

  /**
   * Solution, which naively collects all the points on the lines.
   */
  object NaiveSolution extends Solution {

    def linePoss(line: Line, diagonal: Boolean): IterableOnce[Pos] = {
      val (Pos(x1, y1), Pos(x2, y2)) = line
      if (x1 == x2)
        (y1 directedTo y2).view.map(Pos(x1, _))
      else if (y1 == y2)
        (x1 directedTo x2).view.map(Pos(_, y1))
      else if (diagonal)
        ((x1 directedTo x2) lazyZip (y1 directedTo y2)).map(Pos.apply)
      else
        Iterator.empty
    }

    override def countOverlaps(lines: Seq[Line], diagonal: Boolean = false): Int = {
      lines.iterator
        .flatMap(linePoss(_, diagonal))
        .groupCount(identity)
        .count(_._2 >= 2)
    }
  }

  /**
   * Solution, which only collects pairwise intersections of the lines.
   */
  object IntersectSolution extends Solution {

    def lineIntersect(line1: Line, line2: Line): IterableOnce[Pos] = {
      val (Pos(x1, y1), Pos(x2, y2)) = line1
      val (Pos(x3, y3), Pos(x4, y4)) = line2
      // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
      val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
      val t0 = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
      val u0 = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
      if (d == 0) { // parallel
        if (t0 == 0 && u0 == 0) { // overlapping
          if (y1 == y2)
            (((x1 min x2) max (x3 min x4)) to ((x1 max x2) min (x3 max x4))).view.map(Pos(_, y1))
          else if (x1 == x2)
            (((y1 min y2) max (y3 min y4)) to ((y1 max y2) min (y3 max y4))).view.map(Pos(x1, _))
          else {
            (((x1 min x2) max (x3 min x4)) to ((x1 max x2) min (x3 max x4))).view.map(x =>
              Pos(x, y1 + (x - x1) * (if ((x1 <= x2) == (y1 <= y2)) 1 else -1))
              //Pos(x,( y1 + (x - x1) * ((y2 - y1) / (x2 - x1).toFloat)).round)
            )
          }
        }
        else // not overlapping
          Iterator.empty
      }
      else if ((0 <= t0 && t0 <= d || 0 <= -t0 && -t0 <= -d) && (0 <= u0 && u0 <= d || 0 <= -u0 && -u0 <= -d)) {
        val t = t0 / d.toFloat
        val a = t * (x2 - x1)
        if ((2 * a).round % 2 != 0) { // is close to .5? crossing diagonal lines going through each other!
          Iterator.empty
        }
        else {
          val x = (x1 + a).round
          val y = (y1 + t * (y2 - y1)).round
          Iterator.single(Pos(x, y))
        }
      }
      else
        Iterator.empty
    }

    def lineAxisAligned(line: Line): Boolean = {
      val (Pos(x1, y1), Pos(x2, y2)) = line
      x1 == x2 || y1 == y2
    }

    override def countOverlaps(lines: Seq[Line], diagonal: Boolean = false): Int = {
      val filteredLines = (if (!diagonal) lines.filter(lineAxisAligned) else lines).to(ArraySeq) // ArraySeq for view slicing

      val inters: Iterator[Pos] =
        for {
          // faster than combinations(2)
          (line1, i) <- filteredLines.iterator.zipWithIndex
          line2 <- filteredLines.view.slice(i + 1, filteredLines.size).iterator
          p <- lineIntersect(line1, line2).iterator
        } yield p

      inters.toSet.size
    }
  }


  private val lineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r

  def parseLine(s: String): Line = s match {
    case lineRegex(x1, y1, x2, y2) => (Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
  }

  def parseLines(input: String): Seq[Line] = input.linesIterator.map(parseLine).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import IntersectSolution._

    println(countOverlaps(parseLines(input)))
    println(countOverlaps(parseLines(input), true))
  }
}
