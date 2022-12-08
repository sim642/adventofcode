package eu.sim642.adventofcodelib

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

case class FenwickTree[A](arr: ArraySeq[A])(using op: (A, A) => A) {
  def apply(i: Int): A = {

    @tailrec
    def helper(i: Int, acc: A): A = {
      if (i == 0)
        acc
      else
        helper(i - Integer.lowestOneBit(i), op(acc, arr(i)))
    }

    helper(i, arr(0))
  }

  def updated(i: Int, delta: A): FenwickTree[A] = {

    @tailrec
    def helper(i: Int, arr: ArraySeq[A]): ArraySeq[A] = {
      if (i == 0)
        arr.updated(0, op(arr(0), delta))
      else if (i >= arr.size)
        arr
      else
        helper(i + Integer.lowestOneBit(i), arr.updated(i, op(arr(i), delta)))
    }

    FenwickTree(helper(i, arr))
  }
}

object FenwickTree {
  def fill[A](n: Int)(elem: A)(using op: (A, A) => A)(using ClassTag[A]): FenwickTree[A] =
    FenwickTree(ArraySeq.fill(n)(elem))
}
