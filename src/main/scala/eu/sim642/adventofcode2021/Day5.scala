package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day5 {

  type Line = (Pos, Pos)

  def linePoss(line: Line): IterableOnce[Pos] = line match {
    case (Pos(x1, y1), Pos(x2, y2)) if x1 == x2 => ((y1 min y2) to (y1 max y2)).iterator.map(Pos(x1, _))
    case (Pos(x1, y1), Pos(x2, y2)) if y1 == y2 => ((x1 min x2) to (x1 max x2)).iterator.map(Pos(_, y1))
    case _ => Iterable.empty
  }

  def countOverlaps(lines: Seq[Line]): Int = {
    lines.iterator
      .flatMap(linePoss)
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 >= 2)
  }


  private val lineRegex = """(\d+),(\d+) -> (\d+),(\d+)""".r

  def parseLine(s: String): Line = s match {
    case lineRegex(x1, y1, x2, y2) => (Pos(x1.toInt, y1.toInt), Pos(x2.toInt, y2.toInt))
  }

  def parseLines(input: String): Seq[Line] = input.linesIterator.map(parseLine).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countOverlaps(parseLines(input)))
  }
}
