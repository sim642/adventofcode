package eu.sim642.adventofcodelib

// TODO: in Scala 2.13 convert to IterableOnce
object TraversableOnceImplicits {

  implicit class OrderingTraversableOnceOps[A](coll: TraversableOnce[A]) {
    // TODO: remove in Scala 2.13, it has maxOption already
    def maxOption(implicit ordering: Ordering[A]): Option[A] = coll.reduceOption(ordering.max)
  }
}
