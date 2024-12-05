package eu.sim642.adventofcode2024

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
  }
}
