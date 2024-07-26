package eu.sim642.adventofcodelib.cycle

import scala.collection.mutable

import eu.sim642.adventofcodelib.IteratorImplicits._

object NaiveCycleFinder
  extends IterableOnceCycleFinder
    with IterableOnceCycleByFinder
    with FunctionCycleFinder
    with FunctionCycleByFinder {

  override def find[A](coll: IterableOnce[A]): Option[Cycle[A]] = {
    val prevs = mutable.Map[A, Int]()

    coll.iterator.zipWithPrev
      .zipWithIndex
      .map({ case ((lastX, x), i) => (lastX, x, prevs.put(x, i), i) }) // nasty side-effecting
      .collectFirst({ case (Some(lastX), x, Some(prevI), i) =>
        SimpleCycle(
          stemLength = prevI,
          cycleLength = i - prevI,
          cycleHead = x,
          cycleLast = lastX
        )
      })
  }

  override def find[A](x0: A, f: A => A): Cycle[A] & Indexing[A] = {
    val cycle = find(Iterator.iterate(x0)(f)).get
    FunctionCycle(
      stemLength = cycle.stemLength,
      cycleLength = cycle.cycleLength,
      cycleHead = cycle.cycleHead,
      cycleLast = cycle.cycleLast
    )(x0, f)
  }


  override def findBy[A, B](coll: IterableOnce[A])(m: A => B): Option[CycleBy[A]] = {
    val prevs = mutable.Map[B, (A, Int)]()

    coll.iterator.zipWithPrev
      .zipWithIndex
      .map({ case ((lastX, x), i) => (lastX, x, prevs.put(m(x), (x, i)), i) }) // nasty side-effecting
      .collectFirst({ case (Some(lastX), x, Some((prevX, prevI)), i) =>
        SimpleCycleBy(
          stemLength = prevI,
          cycleLength = i - prevI,
          cycleHead = prevX,
          cycleLast = lastX,
          cycleHeadRepeat = x
        )
      })
  }

  override def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A] = {
    findBy(Iterator.iterate(x0)(f))(m).get
  }
}
