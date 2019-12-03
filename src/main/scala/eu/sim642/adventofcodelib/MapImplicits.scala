package eu.sim642.adventofcodelib

import scala.collection.generic.IsMap
import scala.collection.{BuildFrom, MapOps}
import scala.language.implicitConversions

object MapImplicits {

  class IntersectionMapOps[Repr, M <: IsMap[Repr]](coll: Repr, map: M) {
    // TODO: is BuildFrom even necessary?
    // TODO: generalize that Map
    def intersect[That](thatColl: Repr)(f: (map.V, map.V) => map.V)(implicit bf: BuildFrom[Repr, (map.K, map.V), That]): That = {
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

  implicit def IntersectionMapOps[Repr](coll: Repr)(implicit map: IsMap[Repr]): IntersectionMapOps[Repr, map.type] =
    new IntersectionMapOps(coll, map)
}
