package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.box.Box4
import eu.sim642.adventofcodelib.pos.Pos4

import scala.annotation.tailrec


object Day19 {

  type Category = Char

  case class Part(ratings: Map[Category, Int]) {
    def totalRating: Int = ratings.values.sum
  }

  type PartBox = Box4

  enum Comparison {
    case Lt
    case Gt
  }

  enum Verdict {
    case Accept
    case Reject
    case Continue(workflow: String)
  }

  case class Rule(category: Category, comparison: Comparison, rating: Int, verdict: Verdict) {
    def apply(part: Part): Option[Verdict] = {
      val partRating = part.ratings(category)
      comparison match {
        case Comparison.Lt if partRating < rating => Some(verdict)
        case Comparison.Gt if partRating > rating => Some(verdict)
        case _ => None
      }
    }

    // TODO: simplify
    private val (trueBox, falseBox): (PartBox, PartBox) = (category, comparison) match {
      case ('x', Comparison.Lt) =>
        (Box4(Pos4(1, 1, 1, 1), Pos4(rating - 1, 4000, 4000, 4000)), Box4(Pos4(rating, 1, 1, 1), Pos4(4000, 4000, 4000, 4000)))
      case ('m', Comparison.Lt) =>
        (Box4(Pos4(1, 1, 1, 1), Pos4(4000, rating - 1, 4000, 4000)), Box4(Pos4(1, rating, 1, 1), Pos4(4000, 4000, 4000, 4000)))
      case ('a', Comparison.Lt) =>
        (Box4(Pos4(1, 1, 1, 1), Pos4(4000, 4000, rating - 1, 4000)), Box4(Pos4(1, 1, rating, 1), Pos4(4000, 4000, 4000, 4000)))
      case ('s', Comparison.Lt) =>
        (Box4(Pos4(1, 1, 1, 1), Pos4(4000, 4000, 4000, rating - 1)), Box4(Pos4(1, 1, 1, rating), Pos4(4000, 4000, 4000, 4000)))
      case ('x', Comparison.Gt) =>
        (Box4(Pos4(rating + 1, 1, 1, 1), Pos4(4000, 4000, 4000, 4000)), Box4(Pos4(1, 1, 1, 1), Pos4(rating, 4000, 4000, 4000)))
      case ('m', Comparison.Gt) =>
        (Box4(Pos4(1, rating + 1, 1, 1), Pos4(4000, 4000, 4000, 4000)), Box4(Pos4(1, 1, 1, 1), Pos4(4000, rating, 4000, 4000)))
      case ('a', Comparison.Gt) =>
        (Box4(Pos4(1, 1, rating + 1, 1), Pos4(4000, 4000, 4000, 4000)), Box4(Pos4(1, 1, 1, 1), Pos4(4000, 4000, rating, 4000)))
      case ('s', Comparison.Gt) =>
        (Box4(Pos4(1, 1, 1, rating + 1), Pos4(4000, 4000, 4000, 4000)), Box4(Pos4(1, 1, 1, 1), Pos4(4000, 4000, 4000, rating)))
      case (_, _) => ???
    }

    def apply(partBox: PartBox): Map[PartBox, Option[Verdict]] = {
      val trueMap = (partBox intersect trueBox).map(_ -> Option.apply(verdict)).toMap
      val falseMap = (partBox intersect falseBox).map(_ -> Option.empty[Verdict]).toMap
      trueMap ++ falseMap
    }
  }

  case class Workflow(rules: List[Rule], fallback: Verdict) {
    def apply(part: Part): Verdict = {

      @tailrec
      def helper(rules: List[Rule]): Verdict = rules match {
        case Nil => fallback
        case rule :: newRules => rule(part) match {
          case Some(verdict) => verdict
          case None => helper(newRules)
        }
      }

      helper(rules)
    }

    def apply(partBox: PartBox): Map[PartBox, Verdict] = {

      def helper(rules: List[Rule], partBox: PartBox): Map[PartBox, Verdict] = rules match {
        case Nil => Map(partBox -> fallback)
        case rule :: newRules =>
          rule(partBox).flatMap({
            case (partBox, Some(verdict)) => Map(partBox -> verdict)
            case (partBox, None) => helper(newRules, partBox)
          })
      }

      helper(rules, partBox)
    }
  }

  case class Input(workflows: Map[String, Workflow], parts: Seq[Part]) {
    def apply(part: Part): Boolean = {

      @tailrec
      def helper(workflow: String): Boolean = workflows(workflow)(part) match {
        case Verdict.Accept => true
        case Verdict.Reject => false
        case Verdict.Continue(workflow) => helper(workflow)
      }

      helper("in")
    }

    def apply(partBox: PartBox): Set[PartBox] = {

      def helper(workflow: String, partBox: PartBox): Set[PartBox] = {
        val verdicts = workflows(workflow)(partBox)
        verdicts.flatMap({
          case (partBox, Verdict.Accept) => Set(partBox)
          case (_, Verdict.Reject) => Set.empty
          case (partBox, Verdict.Continue(workflow)) => helper(workflow, partBox)
        }).toSet
      }

      helper("in", partBox)
    }
  }

  def totalAcceptedRating(input: Input): Int =
    input.parts.filter(input(_)).map(_.totalRating).sum

  def countAllAccepted(input: Input): Long = {
    val partBox = Box4(Pos4(1, 1, 1, 1), Pos4(4000, 4000, 4000, 4000))
    input(partBox).map(_.size[Long]).sum
  }


  def parsePart(s: String): Part = s match {
    case s"{$ratings}" =>
      Part(
        ratings.split(',')
          .map({
            case s"$category=$rating" => category.head -> rating.toInt
          })
          .toMap
      )
  }

  def parseVerdict(s: String): Verdict = s match {
    case "A" => Verdict.Accept
    case "R" => Verdict.Reject
    case s => Verdict.Continue(s)
  }

  def parseRule(s: String): Rule = s match {
    case s"$category<$rating:$verdict" =>
      Rule(category.head, Comparison.Lt, rating.toInt, parseVerdict(verdict))
    case s"$category>$rating:$verdict" =>
      Rule(category.head, Comparison.Gt, rating.toInt, parseVerdict(verdict))
  }

  def parseWorkflow(s: String): (String, Workflow) = s match {
    case s"$name{$rules}" =>
      val ruleStrs = rules.split(',')
      name -> Workflow(ruleStrs.init.map(parseRule).toList, parseVerdict(ruleStrs.last))
  }

  def parseInput(input: String): Input = {
    val Seq(workflowsStr, partsStr) = input.split("\n\n").toSeq
    val workflows = workflowsStr.linesIterator.map(parseWorkflow).toMap
    val parts = partsStr.linesIterator.map(parsePart).toSeq
    Input(workflows, parts)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalAcceptedRating(parseInput(input)))
    println(countAllAccepted(parseInput(input)))
  }
}
