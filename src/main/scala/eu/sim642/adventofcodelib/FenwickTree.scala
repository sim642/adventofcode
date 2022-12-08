package eu.sim642.adventofcodelib

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

case class FenwickTree[A](arr: ArraySeq[A])(using op: (A, A) => A) {
  def apply(i: Int): A = {
    var j = i
    var r = arr(0)
    while (j != 0) {
      r = op(r, arr(j))
      j -= Integer.lowestOneBit(j)
    }
    r
  }

  def updated(i: Int, delta: A): FenwickTree[A] = {
    var a = arr
    if (i == 0) {
      a = a.updated(0, op(a(0), delta))
      FenwickTree(a)
    }
    else {
      var j = i
      while (j < arr.size) {
        a = a.updated(j, op(a(j), delta))
        j += Integer.lowestOneBit(j)
      }
      FenwickTree(a)
    }
  }
}

object FenwickTree {
  def fill[A](n: Int)(elem: A)(using op: (A, A) => A)(using ClassTag[A]): FenwickTree[A] =
    FenwickTree(ArraySeq.fill(n)(elem))
}
