package eu.sim642.adventofcode2023

object Day6 {

  case class Race(time: Long, recordDistance: Long) {
    def wins: Long = // TODO: optimize with math
      (0L to time).count(t => t * (time - t) > recordDistance)

    def ++(that: Race): Race =
      Race(
        (time.toString + that.time.toString).toLong,
        (recordDistance.toString + that.recordDistance.toString).toLong
      )
  }

  def multiplyRaceWins(races: Seq[Race]): Long = races.map(_.wins).product

  def concatenatedRaceWin(races: Seq[Race]): Long = races.reduce(_ ++ _).wins


  def parseRaces(input: String): Seq[Race] = input.linesIterator.toSeq match {
    case Seq(s"Time:$timesStr", s"Distance:$distancesStr") =>
      val times = timesStr.trim.split(" +").map(_.toLong).toSeq
      val distances = distancesStr.trim.split(" +").map(_.toLong).toSeq
      (times lazyZip distances).map(Race.apply)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(multiplyRaceWins(parseRaces(input)))
    println(concatenatedRaceWin(parseRaces(input)))
  }
}
