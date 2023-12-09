package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}

object Day7 {

  type Color = String
  type Rules = Map[Color, Map[Color, Int]]

  def countContainingColors(rules: Rules, color: Color = "shiny gold"): Int = {
    val graphTraversal = new GraphTraversal[Color] with UnitNeighbors[Color] {
      override val startNode: Color = color

      override def unitNeighbors(node: Color): IterableOnce[Color] = {
        // TODO: optimize by inverting ahead of time
        for {
          (key, contains) <- rules
          if contains.contains(node)
        } yield key
      }
    }

    BFS.traverse(graphTraversal).nodes.size - 1 // exclude color itself
  }

  def countContainedBags(rules: Rules, color: Color = "shiny gold"): Int = {
    // TODO: optimize by topological sort or helper memoization?
    def helper(color: Color): Int = 1 + rules(color).map({ case (color, count) =>
      count * helper(color)
    }).sum

    helper(color) - 1 // exclude color itself
  }


  private val containRegex = """(\d+) ([a-z ]+) bags?""".r

  def parseContain(s: String): (Color, Int) = s match {
    case containRegex(count, contain) => contain -> count.toInt
  }

  def parseContains(s: String): Map[Color, Int] = s match {
    case "no other bags" => Map.empty
    case s => s.split(", ").map(parseContain).toMap
  }

  private val ruleRegex = """([a-z ]+) bags contain (.*)\.""".r

  def parseRule(s: String): (Color, Map[Color, Int]) = s match {
    case ruleRegex(key, contains) => key -> parseContains(contains)
  }

  def parseRules(input: String): Rules = input.linesIterator.map(parseRule).toMap

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countContainingColors(parseRules(input)))
    println(countContainedBags(parseRules(input)))
  }
}
