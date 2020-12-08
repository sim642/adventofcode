package eu.sim642.adventofcodelib.cycle

trait IterableOnceCycleFinder {
  def find[A](coll: IterableOnce[A]): Option[Cycle[A]]
}

trait IterableOnceCycleByFinder {
  def findBy[A, B](coll: IterableOnce[A])(m: A => B): Option[CycleBy[A]]
}

trait FunctionCycleFinder {
  def find[A](x0: A, f: A => A): Cycle[A]
}

trait FunctionCycleByFinder {
  def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A]
}
