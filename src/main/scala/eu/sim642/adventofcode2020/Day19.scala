package eu.sim642.adventofcode2020

import scala.collection.mutable
import scala.util.matching.Regex

object Day19 {

  sealed trait Rule
  case class Literal(char: Char) extends Rule
  case class Sub(i: Int) extends Rule
  case class Concat(left: Rule, right: Rule) extends Rule
  case class Choice(left: Rule, right: Rule) extends Rule

  type Rules = Map[Int, Rule]

  case class Input(rules: Rules, messages: Seq[String])

  def rules2Regex(rules: Rules): Regex = {

    val memo = mutable.Map.empty[Int, String]
    def helper(rule: Rule): String = rule match {
      case Literal(char) => char.toString
      case Sub(i) => memo.getOrElseUpdate(i, helper(rules(i)))
      case Concat(left, right) => helper(left) + helper(right)
      case Choice(left, right) => "(" + helper(left) + "|" + helper(right) + ")"
    }

    helper(Sub(0)).r
  }

  def countMatchingMessages(input: Input): Int = {
    val regex = rules2Regex(input.rules)
    input.messages.count(regex.matches)
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
    println(countMatchingMessages(parseInput(input)))
  }
}
