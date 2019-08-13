package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.pos.Pos

object GridImplicits {

  implicit class PosGridOps[A](grid: Grid[A]) {
    def apply(pos: Pos): A = grid(pos.y)(pos.x)

    def updatedGrid(pos: Pos, elem: A): Grid[A] = {
      grid.updated(pos.y, grid(pos.y).updated(pos.x, elem))
    }

    def containsPos(pos: Pos): Boolean = grid.indices.contains(pos.y) && grid(pos.y).indices.contains(pos.x)

    def posOf(elem: A): Pos = {
      for {
        (row, y) <- grid.zipWithIndex.iterator
        (cell, x) <- row.zipWithIndex.iterator
        if cell == elem
      } return Pos(x, y)

      Pos(-1, -1)
    }
  }
}
