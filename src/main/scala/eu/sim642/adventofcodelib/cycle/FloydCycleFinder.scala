package eu.sim642.adventofcodelib.cycle

object FloydCycleFinder
  extends FunctionCycleFinder
    with FunctionCycleByFinder {

  override def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    // https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_Tortoise_and_Hare
    var tortoise = f(x0)
    var hare = f(f(x0))
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(f(hare))
    }

    var μ = 0
    tortoise = x0
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }

    var λ = 1
    var prevHare = tortoise
    hare = f(tortoise)
    while (tortoise != hare) {
      prevHare = hare
      hare = f(hare)
      λ += 1
    }


    FunctionCycle(
      stemLength = μ,
      cycleLength = λ,
      cycleHead = tortoise,
      cycleLast = prevHare
    )(x0, f)
  }


  override def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A] = {
    var tortoise = f(x0)
    var hare = f(f(x0))
    while (m(tortoise) != m(hare)) {
      tortoise = f(tortoise)
      hare = f(f(hare))
    }

    var μ = 0
    tortoise = x0
    while (m(tortoise) != m(hare)) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }

    var λ = 1
    var prevHare = tortoise
    hare = f(tortoise)
    while (m(tortoise) != m(hare)) {
      prevHare = hare
      hare = f(hare)
      λ += 1
    }


    SimpleCycleBy(
      stemLength = μ,
      cycleLength = λ,
      cycleHead = tortoise,
      cycleLast = prevHare,
      cycleHeadRepeat = hare
    )
  }
}
