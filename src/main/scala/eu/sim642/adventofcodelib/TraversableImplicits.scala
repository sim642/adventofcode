package eu.sim642.adventofcodelib

// TODO: in Scala 2.13 convert to Iterable
object TraversableImplicits {

  implicit class CycleTraversableOps[A](coll: Traversable[A]) {
    def cycle: Iterator[A] = {
      // https://stackoverflow.com/a/2099896
      Iterator.continually(coll).flatten
    }
  }
}
