package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3

object Day2 {

  type Cubes = Pos3

  object Cubes {
    def apply(red: Int = 0, green: Int = 0, blue: Int = 0): Cubes = Pos3(red, green, blue)
  }

  extension (cubes: Cubes) {
    def power: Int = cubes.x * cubes.y * cubes.z
  }

  type Game = Set[Cubes]

  def sumPossibleIds(games: Seq[Game], possible: Cubes = Cubes(12, 13, 14)): Int = {
    games
      .zipWithIndex
      .filter((game, _) => game.forall(_ <= possible))
      .map(_._2 + 1)
      .sum
  }

  def sumPowers(games: Seq[Game]): Int = games.map(_.reduce(_ max _).power).sum


  def parseColor(s: String): Cubes = s match {
    case s"$count red" => Cubes(red = count.toInt)
    case s"$count green" => Cubes(green = count.toInt)
    case s"$count blue" => Cubes(blue = count.toInt)
  }

  def parseCubes(s: String): Cubes = s.split(", ").map(parseColor).reduce(_ + _)

  def parseGame(s: String): Game = s match {
    case s"Game $id: $cubess" => cubess.split("; ").map(parseCubes).toSet
  }

  def parseGames(input: String): Seq[Game] = input.linesIterator.map(parseGame).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPossibleIds(parseGames(input)))
    println(sumPowers(parseGames(input)))
  }
}
