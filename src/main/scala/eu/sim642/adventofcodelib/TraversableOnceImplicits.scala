package eu.sim642.adventofcodelib

// TODO: in Scala 2.13 convert to IterableOnce
object TraversableOnceImplicits {

  implicit class OrderingTraversableOnceOps[A](coll: TraversableOnce[A]) {
    // TODO: remove in Scala 2.13, it has maxOption already
    def maxOption(implicit ordering: Ordering[A]): Option[A] = coll.reduceOption(ordering.max)

    def minStrict(implicit ordering: Ordering[A]): Option[A] = {
      // optional min value, min value duplication flag
      val acc = coll.foldLeft((Option.empty[A], false))({
        case ((None, _), x) => (Some(x), false)
        case (acc@(minOption@Some(min), _), x) =>
          val c = ordering.compare(x, min)
          if (c < 0)
            (Some(x), false)
          else if (c == 0)
            (minOption, true)
          else
            acc
      })

      acc match {
        case (Some(_), true) => None
        case (minOption, _) => minOption
      }
    }
  }
}
