package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day2.HeadIterator

import scala.collection.mutable

object Day12 {

  def simulateGeneration(notes: Map[String, Char])(generation: String, startIndex: Int): (String, Int) = {
    val newGeneration = ("...." + generation + "....").toSeq.sliding(5).map(s => notes(s.unwrap)).mkString("")
    val firstPlantIndex = newGeneration.indexOf("#")
    val lastPlantIndex = newGeneration.lastIndexOf("#")
    (newGeneration.slice(firstPlantIndex, lastPlantIndex + 1), startIndex + 2 - firstPlantIndex)
  }

  def iterateGenerations(initial: String, notes: Map[String, Char]): Iterator[(String, Int)] = {
    Iterator.iterate((initial, 0))(p => simulateGeneration(notes)(p._1, p._2))
  }

  def sumPlants(generation: String, startIndex: Long): Long = {
    generation.zipWithIndex.map({
      case ('.', _) => 0
      case ('#', i) => i - startIndex
    }).sum
  }

  def sumPlantsSimulate(initial: String, notes: Map[String, Char]): Long = {
    val (last, startIndex) = iterateGenerations(initial, notes).drop(20).head
    sumPlants(last, startIndex)
  }

  def sumPlantsSimulate(input: String): Long = {
    val (initial, notes) = parseInput(input)
    sumPlantsSimulate(initial, notes)
  }

  def sumPlantsCycle(initial: String, notes: Map[String, Char], generations: Long = 50000000000L): Long = {
    val prev: mutable.Map[String, (Int, Int)] = mutable.Map.empty

    val it = iterateGenerations(initial, notes)
    for (((generation, startIndex), generationNum) <- it.zipWithIndex) {
      prev.put(generation, (startIndex, generationNum)) match {
        case None =>
        case Some((prevStartIndex, prevGenerationNum)) =>
          val startShift = startIndex - prevStartIndex
          require(generationNum - prevGenerationNum == 1) // works here, otherwise need divmod next
          val cycles = generations - prevGenerationNum
          val lastStartIndex = prevStartIndex + cycles * startShift
          return sumPlants(generation, lastStartIndex)
      }
    }

    ???
  }

  def sumPlantsCycle(input: String): Long = {
    val (initial, notes) = parseInput(input)
    sumPlantsCycle(initial, notes)
  }

  private val initialStateRegex = """initial state: ([.#]+)""".r
  private val noteRegex = """([.#]{5}) => ([.#])""".r

  def parseNote(s: String): (String, Char) = s match {
    case noteRegex(pattern, replace) => (pattern, replace.head)
  }

  def parseInput(input: String): (String, Map[String, Char]) = {
    val initialLine +: _ +: noteLines = input.linesIterator.toSeq
    initialLine match {
      case initialStateRegex(initial) =>
        (initial, noteLines.map(parseNote).toMap.withDefaultValue('.')) // default value for example, where missing
    }
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPlantsSimulate(input))
    println(sumPlantsCycle(input))
  }
}
