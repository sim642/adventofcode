package eu.sim642.adventofcode2015

import scala.collection.mutable

object Day19 {

  def iterateSingleReplacement(replacement: (String, String), s: String): Iterator[String] = {
    val (pattern, repl) = replacement
    val patternRegex = s"(?=$pattern)".r
    patternRegex.findAllMatchIn(s).map(m => s.patch(m.start, repl, pattern.length))
  }

  def iterateSingleReplacements(replacements: Seq[(String, String)], s: String): Iterator[String] = {
    replacements.iterator.flatMap(iterateSingleReplacement(_, s))
  }

  def countDistinctSingleReplacements(replacements: Seq[(String, String)], s: String): Int = {
    iterateSingleReplacements(replacements, s).distinct.size
  }

  def countDistinctSingleReplacements(input: String): Int = {
    val (replacements, s) = parseInput(input)
    countDistinctSingleReplacements(replacements, s)
  }

  // https://en.wikipedia.org/wiki/Earley_parser
  // https://old.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy6gv3z/
  type Production[N, T] = (N, Seq[Either[N, T]])
  def earleyMin[N, T](grammar: Seq[Production[N, T]], initial: N, input: Seq[T]): Int = {

    def nProductions(n: N): Seq[Production[N, T]] = grammar.filter(_._1 == n)

    case class State(production: Production[N, T], dot: Int, j: Int, count: Int) {
      def isComplete: Boolean = dot >= production._2.length
      def current: Either[N, T] = production._2(dot)
    }

    val S = IndexedSeq.fill(input.length + 1)(mutable.LinkedHashSet.empty[State])
    val initialProductions = nProductions(initial)
    for (production <- initialProductions)
      S(0).add(State(production, 0, 0, 0))

    for (k <- 0 to input.length) {
      val SkQueue = S(k).to(mutable.Queue)

      def addSk(state: State): Unit = {
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

    val parsedCounts = S.last.collect({
      case state@State(production, _, 0, count) if initialProductions.contains(production) && state.isComplete => count
    })
    parsedCounts.min + 1
  }

  private val elemRegex = """[A-Z][a-z]?""".r

  def elemSplit(s: String): Seq[String] = elemRegex.findAllIn(s).toSeq

  def fewestStepsFabricate(replacements: Seq[(String, String)], s: String): Int = {
    val elemReplacements = replacements.map({ case (n, rhs) => (n, elemSplit(rhs)) })
    val elems = elemReplacements.flatMap({ case (n, rhs) => rhs.toSet + n })
    val grammar: Seq[Production[String, String]] = {
      elems.map(elem => (elem, Seq(Right(elem)))) ++
        elemReplacements.map({ case (n, rhs) => (n, rhs.map(Left(_))) })
    }

    val elemS = elemSplit(s)
    earleyMin(grammar, "e", elemS) - elemS.length
  }

  def fewestStepsFabricate(input: String): Int = {
    val (replacements, s) = parseInput(input)
    fewestStepsFabricate(replacements, s)
  }


  private val replacementRegex = """(\w+) => (\w+)""".r
  private val inputRegex = """(?s)(.*)\n\n(.*)""".r

  def parseReplacement(s: String): (String, String) = s match {
    case replacementRegex(pattern, repl) => (pattern, repl)
  }

  def parseReplacements(s: String): Seq[(String, String)] = s.linesIterator.map(parseReplacement).toSeq

  def parseInput(input: String): (Seq[(String, String)], String) = input match {
    case inputRegex(replacements, s) => (parseReplacements(replacements), s)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countDistinctSingleReplacements(input))
    println(fewestStepsFabricate(input))
  }
}
