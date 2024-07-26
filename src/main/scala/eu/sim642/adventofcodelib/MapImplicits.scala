package eu.sim642.adventofcodelib

import scala.collection.generic.IsMap
import scala.collection.{BuildFrom, MapOps}

object MapImplicits {

  extension [Repr](coll: Repr)(using map: IsMap[Repr]) {
    // TODO: is BuildFrom even necessary?
    // TODO: generalize that Map
    infix def intersect[That](thatColl: Repr)(f: (map.V, map.V) => map.V)(using bf: BuildFrom[Repr, (map.K, map.V), That]): That = {
      val thisOps = map(coll)
      val thatOps = map(thatColl)
      bf.fromSpecific(coll)(
        /*thisOps.view
          .filterKeys(thatOps.contains)
          .toMap
          .transform({ case (k, v) =>
            f(v, thatOps(k))
          })*/
        thisOps.view
          .filterKeys(thatOps.contains)
          .map({ case (k, v) =>
            k -> f(v, thatOps(k))
          })
      )
    }
  }
}
