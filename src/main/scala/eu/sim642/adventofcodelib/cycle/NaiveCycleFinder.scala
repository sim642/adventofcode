package eu.sim642.adventofcodelib.cycle

import scala.collection.mutable

import eu.sim642.adventofcodelib.IteratorImplicits._

object NaiveCycleFinder {
  def find[A](it: Iterator[A]): Cycle[A] = {
    val prevs = mutable.Map[A, Int]()

    val (Some(lastX), x, Some(prevI), i) =
      it.zipWithPrev
        .zipWithIndex
        .map({ case ((lastX, x), i) => (lastX, x, prevs.put(x, i), i) }) // nasty side-effecting
        .find(_._3.isDefined).get

    SimpleCycle(
      stemLength = prevI,
      cycleLength = i - prevI,
      cycleHead = x,
      cycleLast = lastX
    )
  }

  def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    val cycle = find(Iterator.iterate(x0)(f))
    FunctionCycle(
      stemLength = cycle.stemLength,
      cycleLength = cycle.cycleLength,
      cycleHead = cycle.cycleHead,
      cycleLast = cycle.cycleLast
    )(x0, f)
  }


  def findBy[A, B](it: Iterator[A])(m: A => B): CycleBy[A] = {
    val prevs = mutable.Map[B, (A, Int)]()

    val (Some(lastX), x, Some((prevX, prevI)), i) =
      it.zipWithPrev
        .zipWithIndex
        .map({ case ((lastX, x), i) => (lastX, x, prevs.put(m(x), (x, i)), i) }) // nasty side-effecting
        .find(_._3.isDefined).get

    SimpleCycleBy(
      stemLength = prevI,
      cycleLength = i - prevI,
      cycleHead = prevX,
      cycleLast = lastX,
      cycleHeadRepeat = x
    )
  }

  def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A] = {
    findBy(Iterator.iterate(x0)(f))(m)
  }
}
