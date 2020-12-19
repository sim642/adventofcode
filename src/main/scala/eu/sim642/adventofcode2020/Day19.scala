package eu.sim642.adventofcode2020

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

object Day19 {

  sealed trait Rule
  case class Literal(char: Char) extends Rule
  case class Sub(i: Int) extends Rule
  case class Concat(left: Rule, right: Rule) extends Rule
  case class Choice(left: Rule, right: Rule) extends Rule

  type Rules = Map[Int, Rule]

  case class Input(rules: Rules, messages: Seq[String])

  private val fixedRules = Map(
    8 -> Choice(Sub(42), Concat(Sub(42), Sub(8))),
    11 -> Choice(Concat(Sub(42), Sub(31)), Concat(Concat(Sub(42), Sub(11)), Sub(31)))
  )

  sealed trait Solution {
    def rules2Predicate(rules: Rules): String => Boolean

    def countMatchingMessages(input: Input): Int = {
      val predicate = rules2Predicate(input.rules)
      input.messages.count(predicate)
    }

    def countMatchingMessagesFixed(input: Input): Int = {
      countMatchingMessages(input.copy(rules = input.rules ++ fixedRules))
    }
  }

  object RegexSolution extends Solution {
    override def rules2Predicate(rules: Rules): String => Boolean = {
      val memo = mutable.Map.empty[Int, String]

      def helper(rule: Rule): String = rule match {
        case Literal(char) => char.toString
        case Sub(i) => memo.getOrElseUpdate(i, helper(rules(i)))
        case Concat(left, right) => helper(left) + helper(right)
        case Choice(left, right) => "(" + helper(left) + "|" + helper(right) + ")"
      }

      val regex = helper(Sub(0)).r
      regex.matches
    }
  }

  object ParserSolution extends Solution with RegexParsers {

    // TODO: figure out why parser combinators didn't work right
    override def rules2Predicate(rules: Rules): String => Boolean = {
      val memo = mutable.Map.empty[Int, Parser[Unit]] // TODO: is this necessary for Parser?

      def helper(rule: Rule): Parser[Unit] = rule match {
        case Literal(char) => literal(char.toString) ^^^ ()
        case Sub(i) => memo.getOrElseUpdate(i, helper(rules(i)))
        case Concat(left, right) => helper(left) ~ helper(right) ^^^ ()
        case Choice(left, right) => helper(left) | helper(right)
      }

      val parser = helper(Sub(0))
      parseAll(parser, _).successful
    }
  }

  object EarleySolution extends Solution {

    // copied & modified from 2015 Day 19
    // TODO: move earley parser to library
    // https://en.wikipedia.org/wiki/Earley_parser
    // https://old.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy6gv3z/
    type Production[N, T] = (N, Seq[Either[N, T]])
    def earley[N, T](grammar: Seq[Production[N, T]], initial: N, input: Seq[T]): Boolean = {

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

      S.last.exists(s => s.isComplete && initialProductions.contains(s.production))
    }

    override def rules2Predicate(rules: Rules): String => Boolean = {

      def helper(rule: Rule): Seq[Seq[Either[Int, Char]]] = rule match {
        case Literal(char) => Seq(Seq(Right(char)))
        case Sub(i) => Seq(Seq(Left(i)))
        case Concat(left, right) =>
          for {
            l <- helper(left)
            r <- helper(right)
          } yield l ++ r
        case Choice(left, right) => helper(left) ++ helper(right)
      }

      def helper2(i: Int, rule: Rule): Seq[Production[Int, Char]] = helper(rule).map(i -> _)

      val grammar = rules.toSeq.flatMap({ case (i, rule) => helper2(i, rule) })
      earley(grammar, 0, _)
    }
  }


  private val ruleRegex = """(\d+): (.*)""".r
  private val literalRegex = """"(.)"""".r

  def parseRule(s: String): (Int, Rule) = s match {
    case ruleRegex(i, rule) =>
      i.toInt -> (rule match {
        case literalRegex(char) =>
          Literal(char.head)
        case rule =>
          rule
            .split(" \\| ")
            .toSeq
            .map(choice =>
              choice
                .split(" ")
                .toSeq
                .map(i =>
                  Sub(i.toInt)
                )
                .reduce(Concat)
            )
            .reduce(Choice)
      })
  }

  def parseRules(s: String): Rules = s.linesIterator.map(parseRule).toMap

  def parseInput(input: String): Input = {
    val Seq(rules, messages) = input.split("\n\n").toSeq
    Input(parseRules(rules), messages.linesIterator.toSeq)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import EarleySolution._

    println(countMatchingMessages(parseInput(input)))
    println(countMatchingMessagesFixed(parseInput(input)))
  }
}
