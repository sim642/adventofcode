package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

object Day11 {

  def sumDistances(galaxies: Seq[Pos]): Int = {
    val xs = galaxies.map(_.x).toSet
    val freeXs = (0 to xs.max).toSet -- xs
    val ys = galaxies.map(_.y).toSet
    val freeYs = (0 to ys.max).toSet -- ys

    (for {
      (galaxy1, i) <- galaxies.view.zipWithIndex
      galaxy2 <- galaxies.view.drop(i + 1)
      galaxyMin = galaxy1 min galaxy2
      galaxyMax = galaxy1 max galaxy2
    } yield (galaxy1 manhattanDistance galaxy2) + freeXs.count(x => galaxyMin.x < x && x < galaxyMax.x) + freeYs.count(y => galaxyMin.y < y && y < galaxyMax.y)).sum
  }

  // TODO: deduplicate
  def sumDistances2(galaxies: Seq[Pos], factor: Long = 1000000L): Long = {
    val xs = galaxies.map(_.x).toSet
    val freeXs = (0 to xs.max).toSet -- xs
    val ys = galaxies.map(_.y).toSet
    val freeYs = (0 to ys.max).toSet -- ys

    (for {
      (galaxy1, i) <- galaxies.view.zipWithIndex
      galaxy2 <- galaxies.view.drop(i + 1)
      galaxyMin = galaxy1 min galaxy2
      galaxyMax = galaxy1 max galaxy2
    } yield (galaxy1 manhattanDistance galaxy2) + (factor - 1) * freeXs.count(x => galaxyMin.x < x && x < galaxyMax.x) + (factor - 1) * freeYs.count(y => galaxyMin.y < y && y < galaxyMax.y)).sum
  }

  def parseGalaxies(input: String): Seq[Pos] = {
    val grid = input.linesIterator.map(_.toVector).toVector
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '#'
    } yield Pos(x, y)).toSeq
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumDistances(parseGalaxies(input)))
    println(sumDistances2(parseGalaxies(input)))
  }
}
