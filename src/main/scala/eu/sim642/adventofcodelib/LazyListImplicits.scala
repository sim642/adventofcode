package eu.sim642.adventofcodelib

object LazyListImplicits {

  implicit class LazyListUnfoldOps(lazyList: LazyList.type) {
    // https://github.com/tpolecat/examples/blob/ab444af9101b9049d6bd7ebf13ae583bc77ac60a/src/main/scala/eg/Unfold.scala
    // converted to Scala 2.13 LazyList, unfold now standard (but with return arguments swapped)
    def unfold0[A](a: A)(f: A => Option[A]): LazyList[A] =
      LazyList.unfold(a)(a => f(a).map(a => (a, a)))
  }

  implicit class CycleLazyListOps[A](coll: LazyList[A]) {
    def cycle: LazyList[A] = {
      // copied from IterableImplicits (with more refined return type)
      LazyList.continually(coll).flatten
    }
  }
}
