package eu.sim642.adventofcode2023

object Day6 {

  case class Race(time: Int, recordDistance: Int) {
    def wins: Int = {
      (0 to time).count(t => t * (time - t) > recordDistance)
    }
  }

  def multiplyRaceWins(races: Seq[Race]): Int = races.map(_.wins).product


  def parseRaces(input: String): Seq[Race] = input.linesIterator.toSeq match {
    case Seq(s"Time:$timesStr", s"Distance:$distancesStr") =>
      val times = timesStr.trim.split(" +").map(_.toInt).toSeq
      val distances = distancesStr.trim.split(" +").map(_.toInt).toSeq
      (times lazyZip distances).map(Race.apply)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(multiplyRaceWins(parseRaces(input)))
  }
}
