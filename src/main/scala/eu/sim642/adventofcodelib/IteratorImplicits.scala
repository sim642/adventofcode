package eu.sim642.adventofcodelib

import scala.collection.{AbstractIterator, immutable}

object IteratorImplicits {

  implicit class IndexIteratorOps[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next()) else None
    def head: A = headOption.get

    def lastOption: Option[A] = it.reduceOption((_, x) => x)
    def last: A = lastOption.get

    def apply(i: Int): A = it.drop(i).head
  }

  implicit class ZipIteratorOps[A](it: Iterator[A]) {
    def zipWithTail: Iterator[(A, A)] = {
      if (it.hasNext) {
        // TODO: can be done with unfold for Iterator?
        new AbstractIterator[(A, A)] {
          private var prev: A = it.next()

          override def hasNext: Boolean = it.hasNext

          override def next(): (A, A) = {
            val cur = it.next()
            val ret = (prev, cur)
            prev = cur
            ret
          }
        }
      }
      else
        Iterator.empty
    }

    /*def zipTail: Iterator[(A, A)] = {
      it.sliding(2).map({ case Seq(a, b) => (a, b) }) // simpler but slower (by ~3s)
    }*/

    def zipWithPrev: Iterator[(Option[A], A)] = new AbstractIterator[(Option[A], A)] {
      private var prevOption: Option[A] = None

      override def hasNext: Boolean = it.hasNext

      override def next(): (Option[A], A) = {
        val cur = it.next()
        val ret = (prevOption, cur)
        prevOption = Some(cur)
        ret
      }
    }
  }

  implicit class GroupIteratorOps[A](it: Iterator[A]) {
    // TODO: Remove if Scala gets groupMapReduce on Iterator
    // Signature copied from Iterable
    def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): immutable.Map[K, B] = {
      it.to(LazyList).groupMapReduce(key)(f)(reduce)
    }

    // Copied from IterableImplicits
    def groupCount[K](key: A => K): Map[K, Int] = {
      it.groupMapReduce(key)(_ => 1)(_ + _)
    }
  }

  implicit class IteratorUnfoldOps(iterator: Iterator.type) {
    // copied from LazyListImplicits
    def unfold0[A](a: A)(f: A => Option[A]): Iterator[A] =
      Iterator.unfold(a)(a => f(a).map(a => (a, a)))
  }
}
