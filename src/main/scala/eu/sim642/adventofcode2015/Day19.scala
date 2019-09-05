package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, TargetNode, UnitNeighbors}

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

  def fewestStepsFabricate(replacements: Seq[(String, String)], s: String): Int = {
    val graphSearch = new GraphSearch[String] with UnitNeighbors[String] with TargetNode[String] {
      override val startNode: String = "e"

      override def unitNeighbors(molecule: String): IterableOnce[String] = iterateSingleReplacements(replacements, molecule)

      override val targetNode: String = s
    }

    BFS.search(graphSearch).target.get._2
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
    // TODO: optimize
    //println(fewestStepsFabricate(input))
  }
}
