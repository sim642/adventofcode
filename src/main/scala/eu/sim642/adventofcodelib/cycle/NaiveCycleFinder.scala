package eu.sim642.adventofcodelib.cycle

import scala.collection.mutable

object NaiveCycleFinder {
  def find[A](it: Iterator[A]): Cycle[A] = {
    val prevs = mutable.Map[A, Int]()

    val (x, Some(prevI), i) =
      it.zipWithIndex
        .map({ case (x, i) => (x, prevs.put(x, i), i) }) // nasty side-effecting
        .find(_._2.isDefined).get

    SimpleCycle(
      stemLength = prevI,
      cycleLength = i - prevI,
      cycleHead = x
    )
  }

  def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    val cycle = find(Iterator.iterate(x0)(f))
    FunctionCycle(
      stemLength = cycle.stemLength,
      cycleLength = cycle.cycleLength,
      cycleHead = cycle.cycleHead
    )(x0, f)
  }
}
