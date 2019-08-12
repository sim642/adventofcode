package eu.sim642.adventofcodelib.cycle

import scala.collection.mutable

object NaiverCycleFinder {
  def find[A](it: Iterator[A]): Cycle[A] with Indexing[A] = {
    val prevs = mutable.Map[A, Int]()
    val values = Vector.newBuilder[A]

    val (x, Some(prevI), i) =
      it.zipWithIndex
        .map({ case (x, i) =>
          values += x // nasty side-effecting
          (x, prevs.put(x, i), i) // nasty side-effecting
        })
        .find(_._2.isDefined).get

    val (stem, cycle) = values.result().init.splitAt(prevI) // TODO: don't split and use stemCycle directly?
    VectorCycle(stem, cycle)
  }

  def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    find(Iterator.iterate(x0)(f))
  }
}
