package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day20 {

  implicit class OrientationGridOps[A](grid: Grid[A]) {
    def rotate: Grid[A] = grid.transpose.reverse
    def flip: Grid[A] = grid.reverse

    def orientations: Seq[Grid[A]] = Seq(
      grid,
      rotate,
      rotate.rotate,
      rotate.rotate.rotate,
      flip,
      flip.rotate,
      flip.rotate.rotate,
      flip.rotate.rotate.rotate,
    )
  }

  type Border = Vector[Boolean]

  case class Tile(id: Int, grid: Grid[Boolean]) {
    def borderTop: Border = grid.head
    def borderBottom: Border = grid.last
    def borderLeft: Border = grid.map(_.head)
    def borderRight: Border = grid.map(_.last)

    def orientations: Seq[Tile] = grid.orientations.map(grid => copy(grid = grid))

    def innerGrid: Grid[Boolean] = grid.drop(1).dropRight(1).map(_.drop(1).dropRight(1))

    override def toString: String = id.toString
  }

  def getBorderTiles(tiles: Seq[Tile]): Map[Border, Seq[Tile]] = {
    def borders(tile: Tile): Seq[Border] = {
      Seq(
        tile.borderTop,
        tile.borderBottom,
        tile.borderLeft,
        tile.borderRight
      )
        .flatMap(border =>
          Seq(border, border.reverse)
        )
    }

    tiles
      .flatMap(tile =>
        borders(tile).map(_ -> tile)
      )
      .groupMap(_._1)(_._2)
  }

  def getTileEdgeBorders(borderTiles: Map[Border, Seq[Tile]]): Map[Tile, Set[Border]] = {
    val edgeBorderTiles =
      borderTiles
        .collect({
          case (border, Seq(tile)) => border -> tile
        })

    edgeBorderTiles.groupMapReduce(_._2)(p => Set(p._1))(_ ++ _)
  }

  def findCorners(borderTiles: Map[Border, Seq[Tile]]): Seq[Tile] = {
    getTileEdgeBorders(borderTiles)
      .filter(_._2.size == 4)
      .keys
      .toSeq
  }

  def findCorners(tiles: Seq[Tile]): Seq[Tile] = findCorners(getBorderTiles(tiles))

  def cornerIdProduct(tiles: Seq[Tile]): Long = findCorners(tiles).map(_.id.toLong).product

  def solvePuzzle(tiles: Seq[Tile]): Grid[Tile] = {
    val borderTiles = getBorderTiles(tiles)

    def solveRow(tileLeft: Tile): Vector[Tile] = {
      val border = tileLeft.borderRight
      //val newTiles = borderTiles(border).toSet - tileLeft
      val newTiles = borderTiles(border).toSet.filterNot(_.id == tileLeft.id)
      if (newTiles.size == 1) {
        val newTile = newTiles.head
        val newTileOriented = newTile.orientations.find(_.borderLeft == border).get
        solveRow1(newTileOriented)
      }
      else if (newTiles.isEmpty) {
        Vector.empty
      }
      else
        throw new IllegalArgumentException("ambiguous new tile")
    }

    def solveRow1(tileLeft: Tile): Vector[Tile] = tileLeft +: solveRow(tileLeft)

    def solve(tileTop: Tile): Grid[Tile] = {
      val border = tileTop.borderBottom
      //val newTiles = borderTiles(border).toSet - tileLeft
      val newTiles = borderTiles(border).toSet.filterNot(_.id == tileTop.id)
      if (newTiles.size == 1) {
        val newTile = newTiles.head
        val newTileOriented = newTile.orientations.find(_.borderTop == border).get
        solve1(newTileOriented)
      }
      else if (newTiles.isEmpty) {
        Vector.empty
      }
      else
        throw new IllegalArgumentException("ambiguous new tile")
    }

    def solve1(tileTop: Tile): Grid[Tile] = solveRow1(tileTop) +: solve(tileTop)

    val corners = findCorners(borderTiles)
    val cornerTopLeft = corners.head
    val cornerTopLeftBorders = getTileEdgeBorders(borderTiles)(cornerTopLeft) // TODO: don't repeat with findCorners
    val cornerTopLeftOriented = cornerTopLeft.orientations.find(tile => cornerTopLeftBorders.contains(tile.borderTop) && cornerTopLeftBorders.contains(tile.borderLeft)).get

    solve1(cornerTopLeftOriented)
  }

  private val seaMonster =
    """                  #.
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  def checkSeaMonsters(tiles: Seq[Tile]): Int = {
    val solved = solvePuzzle(tiles)
    val grid = solved.mapGrid(_.innerGrid).flattenGrid

    // TODO: move this to library?
    def booleanGrid2Poss(grid: Grid[Boolean]): Set[Pos] = {
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell
      } yield Pos(x, y)).toSet
    }

    val gridPoss = booleanGrid2Poss(grid)
    val gridSize = Pos(grid.head.length, grid.length)

    // orienting sea monster is slightly faster than orienting grid

    def checkGridSeaMonster(seaMonsterGrid: Grid[Boolean]): Option[Int] = {
      val seaMonsterPoss = booleanGrid2Poss(seaMonsterGrid)
      val seaMonsterSize = Pos(seaMonsterGrid.head.length, seaMonsterGrid.length)

      Box(Pos.zero, gridSize - seaMonsterSize - Pos(1, 1)).iterator
        .map(pos => seaMonsterPoss.map(pos + _))
        .filter(_.subsetOf(gridPoss))
        .reduceOption(_ ++ _)
        .map(seaMonsterPoss => (gridPoss -- seaMonsterPoss).size)
    }

    val seaMonsterGrid = seaMonster.linesIterator.map(_.toVector).toVector.mapGrid(_ == '#')
    seaMonsterGrid.orientations.flatMap(checkGridSeaMonster).head
  }

  private val tileIdRegex = """Tile (\d+):""".r

  def parseTile(s: String): Tile = {
    val tileIdLine +: gridLines = s.linesIterator.toVector: @unchecked
    val grid = gridLines.map(_.toVector).mapGrid(_ == '#')
    tileIdLine match {
      case tileIdRegex(id) => Tile(id.toInt, grid)
    }
  }

  def parseTiles(input: String): Seq[Tile] = input.split("\n\n").toSeq.map(parseTile)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(cornerIdProduct(parseTiles(input)))
    println(checkSeaMonsters(parseTiles(input)))
  }
}
