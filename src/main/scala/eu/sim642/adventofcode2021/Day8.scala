package eu.sim642.adventofcode2021

import scala.collection.immutable.BitSet

object Day8 {

  type Signals = BitSet

  def parseSignals(s: String): Signals = s.map(_ - 'a').to(BitSet)

  private val digitSignals: Map[Int, Signals] = Map(
    0 -> parseSignals("abcefg"),
    1 -> parseSignals("cf"),
    2 -> parseSignals("acdeg"),
    3 -> parseSignals("acdfg"),
    4 -> parseSignals("bcdf"),
    5 -> parseSignals("abdfg"),
    6 -> parseSignals("abdefg"),
    7 -> parseSignals("acf"),
    8 -> parseSignals("abcdefg"),
    9 -> parseSignals("abcdfg"),
  )

  private val signalsDigits: Map[Signals, Int] = digitSignals.map(_.swap)


  case class Entry(patterns: Seq[Signals], outputs: Seq[Signals])

  private val uniqueSignalsLengths = Set(2, 3, 4, 7)

  def isUnique(signals: Signals): Boolean = uniqueSignalsLengths.contains(signals.size)

  def countUniqueOutputs(entries: Seq[Entry]): Int = {
    entries.iterator
      .flatMap(_.outputs)
      .count(isUnique)
  }

  def decodeEntry(entry: Entry): Int = {
    // TODO: optimize, something smarter than all permutations
    val permutation = (0 to 6).permutations
      .find(permutation => {
        val patterns2 = entry.patterns.map(_.map(permutation))
        patterns2.toSet == digitSignals.values.toSet
      })
      .get

    val outputs2 = entry.outputs.map(_.map(permutation))
    outputs2.map(signalsDigits).foldLeft(0)((acc, digit) => 10 * acc + digit)
  }

  def sumDecodeEntries(entries: Seq[Entry]): Int = entries.map(decodeEntry).sum


  def parseEntry(s: String): Entry = {
    val Array(patternsStr, outputsStr) = s.split(" \\| ", 2)
    val patterns = patternsStr.split(" ").toSeq.map(parseSignals)
    val outputs = outputsStr.split(" ").toSeq.map(parseSignals)
    Entry(patterns, outputs)
  }

  def parseEntries(input: String): Seq[Entry] = input.linesIterator.map(parseEntry).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countUniqueOutputs(parseEntries(input)))
    println(sumDecodeEntries(parseEntries(input)))
  }
}
