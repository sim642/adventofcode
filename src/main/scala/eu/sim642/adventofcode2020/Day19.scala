package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.grammar._

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

  object CombinatorParserSolution extends Solution with RegexParsers {

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

  object ManualParserSolution extends Solution {
    override def rules2Predicate(rules: Rules): String => Boolean = {

      // Iterator instead of Option allows necessary backtracking
      def helper(rule: Rule, chars: List[Char]): Iterator[List[Char]] = rule match {
        case Literal(char) => chars match {
          case firstChar :: restChars if firstChar == char => Iterator(restChars)
          case _ => Iterator.empty
        }
        case Sub(i) => helper(rules(i), chars)
        case Concat(left, right) => helper(left, chars).flatMap(helper(right, _))
        case Choice(left, right) => helper(left, chars) ++ helper(right, chars)
      }

      message => helper(Sub(0), message.toList).contains(Nil)
    }
  }

  object EarleySolution extends Solution {
    override def rules2Predicate(rules: Rules): String => Boolean = {

      def helper(rule: Rule): Set[ProductionBody[Int, Char]] = rule match {
        case Literal(char) => Set(Seq(Right(char)))
        case Sub(i) => Set(Seq(Left(i)))
        case Concat(left, right) =>
          for {
            l <- helper(left)
            r <- helper(right)
          } yield l ++ r
        case Choice(left, right) => helper(left) ++ helper(right)
      }

      val grammar: Grammar[Int, Char] = rules.view.mapValues(helper).toMap
      Earley.matches(grammar, 0, _)
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
                .reduce(Concat.apply)
            )
            .reduce(Choice.apply)
      })
  }

  def parseRules(s: String): Rules = s.linesIterator.map(parseRule).toMap

  def parseInput(input: String): Input = {
    val Seq(rules, messages) = input.split("\n\n").toSeq
    Input(parseRules(rules), messages.linesIterator.toSeq)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import ManualParserSolution._

    println(countMatchingMessages(parseInput(input)))
    println(countMatchingMessagesFixed(parseInput(input)))
  }
}
