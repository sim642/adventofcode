package eu.sim642.adventofcode2025

import eu.sim642.adventofcode2016.Day20.Interval

object Day5 {

  case class Database(fresh: Seq[Interval], available: Seq[Long])

  def countFreshAvailable(database: Database): Int = {
    val Database(fresh, available) = database
    available.count(id => fresh.exists(_.contains(id)))
  }

  def parseInterval(s: String): Interval = s match {
    case s"$i-$j" => Interval(i.toLong, j.toLong)
  }

  def parseDatabase(input: String): Database = input match {
    case s"$freshStr\n\n$availableStr" =>
      val fresh = freshStr.linesIterator.map(parseInterval).toSeq
      val available = availableStr.linesIterator.map(_.toLong).toSeq
      Database(fresh, available)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countFreshAvailable(parseDatabase(input)))
  }
}
