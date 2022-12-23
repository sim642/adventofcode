package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.pos.Pos

import scala.util.control.NonLocalReturns._

object GridImplicits {

  implicit class PosGridOps[A](grid: Grid[A]) {
    def apply(pos: Pos): A = grid(pos.y)(pos.x)

    def updatedGrid(pos: Pos, elem: A): Grid[A] = {
      grid.updated(pos.y, grid(pos.y).updated(pos.x, elem))
    }

    def containsPos(pos: Pos): Boolean = {
      0 <= pos.x && 0 <= pos.y && pos.y < grid.size && pos.x < grid(pos.y).size
    }

    def posOf(elem: A): Pos = returning {
      for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == elem
      } throwReturn(Pos(x, y))

      Pos(-1, -1)
    }
  }

  implicit class CollectionGridOps[A](grid: Grid[A]) {
    def countGrid(p: A => Boolean): Int = grid.map(_.count(p)).sum

    def mapGrid[B](f: A => B): Grid[B] = grid.map(_.map(f))

    def flattenGrid[B](using asGrid: A => Grid[B]): Grid[B] =
      grid.mapGrid(asGrid).flatMap(_.transpose.map(_.flatten))

    def groupedGrid(groupSize: Int): Grid[Grid[A]] =
      grid.grouped(groupSize).map(_.map(_.grouped(groupSize).toVector).transpose).toVector

    def slidingGrid(size: Pos): Iterator[Iterator[Grid[A]]] = {
      grid.sliding(size.y).map({ rows =>
        rows.map(_.sliding(size.x).toVector).transpose.iterator
      })
    }

    def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = slidingGrid(Pos(size, size))

    def correspondsGrid[B](otherGrid: Grid[B])(p: (A, B) => Boolean): Boolean =
      grid.corresponds(otherGrid)(_.corresponds(_)(p))

    def sumGrid(using Numeric[A]): A =
      grid.iterator.map(_.sum).sum

    def sizeGrid: Int = grid.size * grid(0).size // assumes rectangular grid!

    def padGrid(elem: A): Grid[A] = {
      val maxWidth = grid.view.map(_.length).max
      grid.map(_.padTo(maxWidth, elem))
    }
  }
}
