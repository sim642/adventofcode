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


  def findBy[A, B](it: Iterator[A])(m: A => B): CycleBy[A] = {
    val prevs = mutable.Map[B, (A, Int)]()

    val (x, Some((prevX, prevI)), i) =
      it.zipWithIndex
        .map({ case (x, i) => (x, prevs.put(m(x), (x, i)), i) }) // nasty side-effecting
        .find(_._2.isDefined).get

    SimpleCycleBy(
      stemLength = prevI,
      cycleLength = i - prevI,
      cycleHead = prevX,
      cycleHeadRepeat = x
    )
  }

  def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A] = {
    findBy(Iterator.iterate(x0)(f))(m)
  }
}
