package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day5 {

  type Line = (Pos, Pos)

  extension (start: Int) {
    def directedTo(end: Int): Range.Inclusive = {
      Range.inclusive(start, end, if (start > end) -1 else 1)
    }
  }

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

  def countOverlaps(lines: Seq[Line], diagonal: Boolean = false): Int = {
    lines.iterator
      .flatMap(linePoss(_, diagonal))
      .groupCount(identity)
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
    println(countOverlaps(parseLines(input), true))
  }
}
