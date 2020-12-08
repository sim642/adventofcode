package eu.sim642.adventofcodelib.cycle

import scala.collection.mutable

object NaiverCycleFinder {
  def find[A](it: Iterator[A]): Option[Cycle[A] with Indexing[A]] = {
    val prevs = mutable.Map[A, Int]()
    val values = Vector.newBuilder[A]

    it.zipWithIndex
      .map({ case (x, i) =>
        values += x // nasty side-effecting
        (x, prevs.put(x, i), i) // nasty side-effecting
      })
      .collectFirst({ case (x, Some(prevI), i) =>
        val (stem, cycle) = values.result().init.splitAt(prevI) // TODO: don't split and use stemCycle directly?
        VectorCycle(stem, cycle)
      })
  }

  def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    find(Iterator.iterate(x0)(f)).get
  }
}
