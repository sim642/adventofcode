package eu.sim642.adventofcode2024

import scala.annotation.tailrec

object Day5 {

  type Rule = (Int, Int)
  type Update = Seq[Int]

  case class Input(rules: Seq[Rule], updates: Seq[Update])

  def isCorrect(rules: Seq[Rule], update: Update): Boolean = {
    rules.forall({ case (x, y) =>
      val i = update.indexOf(x)
      val j = update.indexOf(y)
      i < 0 || j < 0 || i < j
    })
  }

  def sumCorrectMiddles(input: Input): Int = {
    val Input(rules, updates) = input
    updates
      .filter(isCorrect(rules, _))
      .map(update => update(update.size / 2))
      .sum
  }

  def sort(rules: Seq[Rule], update: Update): Update = {
    val updateRules = rules.filter(rule => update.contains(rule._1) && update.contains(rule._2))
    val updateRulesMap = updateRules.toSet.groupMap(_._1)(_._2).withDefaultValue(Set.empty)

    // TODO: optimize?
    @tailrec
    def topologicalSort(pages: Set[Int], updateRulesMap: Map[Int, Set[Int]], sortedUpdate: List[Int]): List[Int] = {
      pages.find(updateRulesMap(_).isEmpty) match {
        case None => sortedUpdate
        case Some(before) =>
          val newUpdateRulesMap = (updateRulesMap - before).view.mapValues(_ - before).toMap
          topologicalSort(pages - before, newUpdateRulesMap, before :: sortedUpdate)
      }
    }

    topologicalSort(update.toSet, updateRulesMap, Nil)
  }

  def sumIncorrectMiddles(input: Input): Int = {
    val Input(rules, updates) = input
    updates
      .filter(!isCorrect(rules, _))
      .map(sort(rules, _))
      .map(update => update(update.size / 2))
      .sum
  }

  def parseRule(s: String): Rule = s match {
    case s"$x|$y" => (x.toInt, y.toInt)
  }

  def parseUpdate(s: String): Update = s.split(",").map(_.toInt).toSeq

  def parseInput(input: String): Input = {
    val Seq(rulesStr, updatesStr) = input.split("\n\n",2).toSeq
    val rules = rulesStr.linesIterator.map(parseRule).toSeq
    val updates = updatesStr.linesIterator.map(parseUpdate).toSeq
    Input(rules, updates)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumCorrectMiddles(parseInput(input)))
    println(sumIncorrectMiddles(parseInput(input)))
  }
}
