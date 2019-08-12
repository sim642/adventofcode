package eu.sim642.adventofcodelib.cycle

object BrentCycleFinder {
  def find[A](x0: A, f: A => A): Cycle[A] with Indexing[A] = {
    // https://en.wikipedia.org/wiki/Cycle_detection#Brent's_algorithm
    var power = 1
    var λ = 1
    var tortoise = x0
    var hare = f(x0)
    while (tortoise != hare) {
      if (power == λ) {
        tortoise = hare
        power *= 2
        λ = 0
      }
      hare = f(hare)
      λ += 1
    }

    tortoise = x0
    hare = x0
    for (i <- 0 until λ)
      hare = f(hare)

    var μ = 0
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }


    FunctionCycle(
      stemLength = μ,
      cycleLength = λ,
      cycleHead = tortoise
    )(x0, f)
  }


  def findBy[A, B](x0: A, f: A => A)(m: A => B): CycleBy[A] = {
    var power = 1
    var λ = 1
    var tortoise = x0
    var hare = f(x0)
    while (m(tortoise) != m(hare)) {
      if (power == λ) {
        tortoise = hare
        power *= 2
        λ = 0
      }
      hare = f(hare)
      λ += 1
    }

    tortoise = x0
    hare = x0
    for (i <- 0 until λ)
      hare = f(hare)

    var μ = 0
    while (m(tortoise) != m(hare)) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }


    SimpleCycleBy(
      stemLength = μ,
      cycleLength = λ,
      cycleHead = tortoise,
      cycleHeadRepeat = hare
    )
  }
}
