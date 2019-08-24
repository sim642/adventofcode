package eu.sim642.adventofcodelib

object IterableImplicits {

  implicit class CycleIterableOps[A](coll: Iterable[A]) {
    def cycle: Iterator[A] = {
      // https://stackoverflow.com/a/2099896
      Iterator.continually(coll).flatten
    }
  }

  implicit class GroupIterableOps[A](coll: Iterable[A]) {
    def groupCount[K](key: A => K): Map[K, Int] = {
      coll.groupMapReduce(key)(_ => 1)(_ + _)
    }
  }
}
