package eu.sim642.adventofcodelib

object IteratorImplicits {

  implicit class IndexIteratorOps[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next) else None
    def head: A = headOption.get

    def lastOption: Option[A] = it.reduceOption((_, x) => x)
    def last: A = lastOption.get

    def apply(i: Int): A = it.drop(i).head
  }
}
