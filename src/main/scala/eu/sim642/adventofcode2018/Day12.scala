package eu.sim642.adventofcode2018

import Day2.HeadIterator

object Day12 {

  def simulateGeneration(notes: Map[String, Char])(generation: String): String = {
    ("...." + generation + "....").sliding(5).map(notes).mkString("")
  }

  def sumPlants(initial: String, notes: Map[String, Char]): Int = {
    val last = Iterator.iterate(initial)(simulateGeneration(notes)).drop(20).head
    println(last)
    val startIndex = 20 * 2
    println(last.drop(startIndex))
    last.zipWithIndex.map({
      case ('.', _) => 0
      case ('#', i) => i - startIndex
    }).sum
  }

  def sumPlants(input: String): Int = {
    val (initial, notes) = parseInput(input)
    sumPlants(initial, notes)
  }

  private val initialStateRegex = """initial state: ([.#]+)""".r
  private val noteRegex = """([.#]{5}) => ([.#])""".r

  def parseNote(s: String): (String, Char) = s match {
    case noteRegex(pattern, replace) => (pattern, replace.head)
  }

  def parseInput(input: String): (String, Map[String, Char]) = {
    val initialLine +: _ +: noteLines = input.lines.toSeq
    initialLine match {
      case initialStateRegex(initial) =>
        (initial, noteLines.map(parseNote).toMap.withDefaultValue('.')) // default value for example, where missing
    }
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPlants(input))
  }
}
