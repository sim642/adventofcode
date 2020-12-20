package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day20 {

  case class Tile(id: Int, grid: Grid[Boolean])

  def findCorners(tiles: Seq[Tile]): Seq[Tile] = {

    def borders(tile: Tile): Seq[Vector[Boolean]] = Seq(
      tile.grid.head,
      tile.grid.last,
      tile.grid.map(_.head),
      tile.grid.map(_.last),
    )

    val edges =
      tiles
        .flatMap(tile =>
          borders(tile)
            .flatMap(border =>
              Seq(border,  border.reverse)
            )
            .map(_ -> tile)
        )
        .groupMap(_._1)(_._2)
        .filter(_._2.size == 1)

    val edgeTiles =
      (for {
        (border, tiles) <- edges.view
        tile <- tiles
      } yield tile -> border)
        .groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)

    for ((a, b) <- edges)
      println(s"$a: ${b.map(_.id)}")

    for ((a, b) <- edgeTiles)
      println(s"${a.id}: ${b.size}")

    edgeTiles.filter(_._2.size == 4).keys.toSeq
  }

  def cornerIdProduct(tiles: Seq[Tile]): Long = findCorners(tiles).map(_.id.toLong).product


  private val tileIdRegex = """Tile (\d+):""".r

  def parseTile(s: String): Tile = {
    val tileIdLine +: gridLines = s.linesIterator.toVector
    val grid = gridLines.map(_.toVector).mapGrid(_ == '#')
    tileIdLine match {
      case tileIdRegex(id) => Tile(id.toInt, grid)
    }
  }

  def parseTiles(input: String): Seq[Tile] = input.split("\n\n").toSeq.map(parseTile)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(cornerIdProduct(parseTiles(input)))
  }
}
