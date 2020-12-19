package eu.sim642.adventofcodelib.grammar

import scala.collection.mutable

object Earley {

  // copied & modified from 2015 Day 19
  // https://en.wikipedia.org/wiki/Earley_parser
  // https://old.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy6gv3z/

  private case class State[N, T](production: Production[N, T], dot: Int, j: Int, count: Int) {
    def isComplete: Boolean = dot >= production._2.length
    def current: Either[N, T] = production._2(dot)
  }

  private def parse[N, T](grammar: Grammar[N, T], initial: N, input: Seq[T]): collection.Set[State[N, T]] = {

    def nProductions(n: N): Seq[Production[N, T]] = grammar.filter(_._1 == n)

    val S = IndexedSeq.fill(input.length + 1)(mutable.LinkedHashSet.empty[State[N, T]])
    val initialProductions = nProductions(initial)
    for (production <- initialProductions)
      S(0).add(State(production, 0, 0, 0))

    for (k <- 0 to input.length) {
      val SkQueue = S(k).to(mutable.Queue)

      def addSk(state: State[N, T]): Unit = {
        if (S(k).add(state))
          SkQueue.enqueue(state)
      }

      while (SkQueue.nonEmpty) {
        val state@State((n, _), _, j, count) = SkQueue.dequeue()
        if (!state.isComplete) {
          state.current match {
            case Left(n) =>
              // prediction
              for (production <- nProductions(n))
                addSk(State(production, 0, k, 0))
            case Right(t) =>
              // scanning
              if (k < input.length && t == input(k))
                S(k + 1).add(state.copy(dot = state.dot + 1))
          }
        }
        else {
          // completion
          for {
            jState <- S(j)
            if !jState.isComplete && jState.current == Left(n)
          } addSk(jState.copy(dot = jState.dot + 1, count = jState.count + 1 + count))
        }
      }
    }

    S.last.collect({
      case state@State(production, _, 0, _) if initialProductions.contains(production) && state.isComplete => state
    })
  }

  def matches[N, T](grammar: Grammar[N, T], initial: N, input: Seq[T]): Boolean = {
    parse(grammar, initial, input).nonEmpty
  }

  def minDerivation[N, T](grammar: Grammar[N, T], initial: N, input: Seq[T]): Int = {
    parse(grammar, initial, input).map(_.count).min + 1
  }
}
