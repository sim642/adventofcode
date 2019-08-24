package eu.sim642.adventofcodelib

object IterableOnceImplicits {

  implicit class OrderingIterableOnceOps[A](coll: IterableOnce[A]) {
    def minStrict(implicit ordering: Ordering[A]): Option[A] = {
      // optional min value, min value duplication flag
      val acc = coll.iterator.foldLeft((Option.empty[A], false))({
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
