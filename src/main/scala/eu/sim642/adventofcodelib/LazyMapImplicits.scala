package eu.sim642.adventofcodelib

import scala.language.implicitConversions

object LazyMapImplicits {

  implicit class LazyCellAnyOps[A](key: A) {
    def ~>[B](value: => B): (A, LazyCell[B]) = key -> LazyCell(value)
  }

  implicit def lazyCell2Any[A](lazyCell: LazyCell[A]): A = lazyCell.get

  implicit def any2LazyCell[A](value: => A): LazyCell[A] = LazyCell(value)
}
