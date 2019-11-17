package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.pos.Pos

object GridImplicits {

  implicit class PosGridOps[A](grid: Grid[A]) {
    def apply(pos: Pos): A = grid(pos.y)(pos.x)

    def updatedGrid(pos: Pos, elem: A): Grid[A] = {
      grid.updated(pos.y, grid(pos.y).updated(pos.x, elem))
    }

    def containsPos(pos: Pos): Boolean = {
      0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size
    }

    def posOf(elem: A): Pos = {
      for {
        (row, y) <- grid.zipWithIndex.iterator
        (cell, x) <- row.zipWithIndex.iterator
        if cell == elem
      } return Pos(x, y)

      Pos(-1, -1)
    }
  }

  implicit class CollectionGridOps[A](grid: Grid[A]) {
    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum

    def mapGrid[B](f: A => B): Grid[B] = grid.map(_.map(f))

    def flattenGrid[B](implicit asGrid: A => Grid[B]): Grid[B] =
      grid.mapGrid(asGrid).flatMap(_.transpose.map(_.flatten))

    def groupedGrid(groupSize: Int): Grid[Grid[A]] =
      grid.grouped(groupSize).map(_.map(_.grouped(groupSize).toVector).transpose).toVector

    def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = {
      grid.sliding(size).map({ rows =>
        rows.map(_.sliding(size).toVector).transpose.iterator
      })
    }

    def sumGrid(implicit num: Numeric[A]): A =
      grid.iterator.map(_.sum).sum
  }
}
