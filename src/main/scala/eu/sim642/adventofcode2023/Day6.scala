package eu.sim642.adventofcode2023

object Day6 {

  case class Race(time: Long, recordDistance: Long) {
    def ++(that: Race): Race =
      Race(
        (time.toString + that.time.toString).toLong,
        (recordDistance.toString + that.recordDistance.toString).toLong
      )
  }

  trait Solution {
    def raceWins(race: Race): Long

    def multiplyRaceWins(races: Seq[Race]): Long = races.map(raceWins).product

    def concatenatedRaceWin(races: Seq[Race]): Long = raceWins(races.reduce(_ ++ _))
  }

  /**
   * Solution, which counts race wins by brute force.
   */
  object NaiveSolution extends Solution {
    override def raceWins(race: Race): Long =
      (0L to race.time).count(t => t * (race.time - t) > race.recordDistance)
  }

  extension (d: Double) {
    def floorExclusive: Double = {
      val dFloor = d.floor
      if (d == dFloor) d - 1 else dFloor
    }

    def ceilExclusive: Double = {
      val dCeil = d.ceil
      if (d == dCeil) d + 1 else dCeil
    }
  }

  /**
   * Solution, which counts race wins by solving a quadratic inequality.
   */
  object QuadraticSolution extends Solution {
    override def raceWins(race: Race): Long = {
      // t * (time - t) > recordDistance
      // -t^2 + time * t - recordDistance > 0
      val d = math.sqrt(race.time * race.time - 4.0 * race.recordDistance)
      // exclusive ceil and floor because inequality is strict
      val minWinTime = ((race.time - d) / 2).ceilExclusive.toLong
      val maxWinTime = ((race.time + d) / 2).floorExclusive.toLong
      maxWinTime - minWinTime + 1
    }
  }


  def parseRaces(input: String): Seq[Race] = input.linesIterator.toSeq match {
    case Seq(s"Time:$timesStr", s"Distance:$distancesStr") =>
      val times = timesStr.trim.split(" +").map(_.toLong).toSeq
      val distances = distancesStr.trim.split(" +").map(_.toLong).toSeq
      (times lazyZip distances).map(Race.apply)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import QuadraticSolution._

    println(multiplyRaceWins(parseRaces(input)))
    println(concatenatedRaceWin(parseRaces(input)))
  }
}
