package eu.sim642.adventofcodelib

import scala.annotation.tailrec

class UnionFind[A](val reprs: Map[A, A]) {
  // TODO: optimize

  def this(items: Seq[A]) = {
    this(items.map(x => x -> x).toMap)
  }

  @tailrec
  final def findRepr(x: A): A = {
    val repr = reprs(x)
    if (x == repr)
      repr
    else
      findRepr(repr)
  }

  def sameRepr(x: A, y: A): Boolean =
    findRepr(x) == findRepr(y)

  def unioned(x: A, y: A): UnionFind[A] = {
    val xRepr = findRepr(x)
    val yRepr = findRepr(y)
    new UnionFind(reprs + (yRepr -> xRepr))
  }

  def groups(): Seq[Seq[A]] =
    reprs.keys.groupBy(findRepr).values.map(_.toSeq).toSeq

  override def toString: String = reprs.toString()
}
