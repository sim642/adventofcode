package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day20 {

  type Border = Vector[Boolean]

  case class Tile(id: Int, grid: Grid[Boolean]) {
    def borderTop: Border = grid.head
    def borderBottom: Border = grid.last
    def borderLeft: Border = grid.map(_.head)
    def borderRight: Border = grid.map(_.last)

    override def toString: String = id.toString
  }

  def findCorners(tiles: Seq[Tile]): Seq[Tile] = {

    def borders(tile: Tile): Seq[Border] = {
      Seq(
        tile.borderTop,
        tile.borderBottom,
        tile.borderLeft,
        tile.borderRight
      )
        .flatMap(border =>
          Seq(border,  border.reverse)
        )
    }

    val borderTiles =
      tiles
        .flatMap(tile =>
          borders(tile).map(_ -> tile)
        )
        .groupMap(_._1)(_._2)
    val edgeBorderTiles =
      borderTiles
        .collect({
          case (border, Seq(tile)) => border -> tile
        })

    val tileEdgeBorders =
      edgeBorderTiles
        .groupMapReduce(_._2)(p => Set(p._1))(_ ++ _)

    /*for ((a, b) <- edgeBorderTiles)
      println(s"$a: $b")

    for ((a, b) <- tileEdgeBorders)
      println(s"${a.id}: ${b.size}")*/

    tileEdgeBorders.filter(_._2.size == 4).keys.toSeq
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
