package eu.sim642.adventofcodelib

object IterableImplicits {

  implicit class CycleIterableOps[A](coll: Iterable[A]) {
    def cycle: Iterator[A] = {
      // https://stackoverflow.com/a/2099896
      Iterator.continually(coll).flatten
    }
  }
}
