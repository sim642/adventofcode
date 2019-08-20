package eu.sim642.adventofcodelib

import scala.math.Ordering.Implicits._

// TODO: in Scala 2.13 convert to IterableOnce
object TraversableOnceImplicits {

  implicit class OrderingTraversableOnceOps[A](coll: TraversableOnce[A]) {
    // TODO: remove in Scala 2.13, it has maxOption already
    def maxOption(implicit ordering: Ordering[A]): Option[A] = coll.reduceOption(ordering.max)

    def minStrict(implicit ordering: Ordering[A]): Option[A] = {
      val minOptions = coll.foldLeft((Option.empty[A], Option.empty[A]))({
        case (minOptions@(_, Some(min2)), x) if min2 <= x => minOptions
        case ((minOption@Some(min), _), x) if min <= x => (minOption, Some(x))
        case ((_, min2Option), x) => (Some(x), min2Option)
      })

      minOptions match {
        case (Some(min), Some(min2)) if min equiv min2 => None // equiv uses ordering, == doesn't!
        case (minOption, _) => minOption
      }
    }
  }
}
