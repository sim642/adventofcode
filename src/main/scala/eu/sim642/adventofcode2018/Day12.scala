package eu.sim642.adventofcode2018

import Day2.HeadIterator
import eu.sim642.adventofcode2017.Day6.FloydSolution

import scala.collection.mutable

object Day12 {

  def simulateGeneration(notes: Map[String, Char])(generation: String, startIndex: Int): (String, Int) = {
    //(("...." + generation + "....").sliding(5).map(notes).mkString(""), startIndex + 2)
    val newGeneration = ("...." + generation + "....").sliding(5).map(notes).mkString("")
    val firstPlantIndex = newGeneration.indexOf("#")
    val lastPlantIndex = newGeneration.lastIndexOf("#")
    (newGeneration.slice(firstPlantIndex, lastPlantIndex + 1), startIndex + 2 - firstPlantIndex)
  }

  def sumPlants(initial: String, notes: Map[String, Char]): Int = {
    val (last, startIndex) = Iterator.iterate((initial, 0))(p => simulateGeneration(notes)(p._1, p._2)).drop(20).head
    last.zipWithIndex.map({
      case ('.', _) => 0
      case ('#', i) => i - startIndex
    }).sum
  }

  def sumPlants(input: String): Int = {
    val (initial, notes) = parseInput(input)
    sumPlants(initial, notes)
  }

  def sumPlants2(initial: String, notes: Map[String, Char]): Long = {
    val prev: mutable.Map[String, (Int, Int)] = mutable.Map.empty

    val it = Iterator.iterate((initial, 0))(p => simulateGeneration(notes)(p._1, p._2))
    var g = 0
    while (true) {
      val (gen, i) = it.next()
      println(s"$g\t$i\t$gen")

      prev.put(gen, (i, g)) match {
        case None =>
        case Some((prevI, prevG)) =>
          val iShift = i - prevI
          // 50000000000
          require(g - prevG == 1)
          val cycles = 50000000000L - prevG
          val finalI = prevI + cycles * iShift

          return gen.zipWithIndex.map({
            case ('.', _) => 0
            case ('#', i) => i - finalI
          }).sum
      }

      g += 1
    }
    /*val (mu, lambda) = FloydSolution.floyd[(String, Int)]((initial, 0), p => simulateGeneration(notes)(p._1, p._2))
    println(mu)
    println(lambda)*/

    ???
  }

  def sumPlants2(input: String): Long = {
    val (initial, notes) = parseInput(input)
    sumPlants2(initial, notes)
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
    //println(sumPlants(input))
    println(sumPlants2(input))
  }
}
