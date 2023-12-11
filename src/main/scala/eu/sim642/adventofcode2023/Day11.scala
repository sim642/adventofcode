package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

object Day11 {

  trait Part {
    val defaultExpansionFactor: Int

    def sumDistances(galaxies: Seq[Pos], expansionFactor: Long = defaultExpansionFactor): Long = {

      def prefixFreeCoords(coords: Set[Int]): IndexedSeq[Int] =
        (0 to coords.max).scanLeft(0)((acc, i) => acc + (if (coords(i)) 0 else 1))

      val prefixFreeXs = prefixFreeCoords(galaxies.map(_.x).toSet)
      val prefixFreeYs = prefixFreeCoords(galaxies.map(_.y).toSet)

      def dist(galaxy1: Pos, galaxy2: Pos) = {
        val galaxyMin = galaxy1 min galaxy2
        val galaxyMax = galaxy1 max galaxy2
        val freeXs = prefixFreeXs(galaxyMax.x) - prefixFreeXs(galaxyMin.x + 1)
        val freeYs = prefixFreeYs(galaxyMax.y) - prefixFreeYs(galaxyMin.y + 1)
        (galaxy1 manhattanDistance galaxy2) + (expansionFactor - 1) * (freeXs + freeYs)
      }

      (for {
        (galaxy1, i) <- galaxies.view.zipWithIndex
        galaxy2 <- galaxies.view.drop(i + 1)
      } yield dist(galaxy1, galaxy2)).sum
    }
  }

  object Part1 extends Part {
    override val defaultExpansionFactor: Int = 2
  }

  object Part2 extends Part {
    override val defaultExpansionFactor: Int = 1000000
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
    println(Part1.sumDistances(parseGalaxies(input)))
    println(Part2.sumDistances(parseGalaxies(input)))
  }
}
