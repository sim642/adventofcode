package eu.sim642.adventofcode2021

object Day8 {

  type Signals = String

  case class Entry(patterns: Seq[Signals], outputs: Seq[Signals])

  private val uniqueSignalsLengths = Set(2, 3, 4, 7)

  def isUnique(signals: Signals): Boolean = uniqueSignalsLengths.contains(signals.length)

  def countUniqueOutputs(entries: Seq[Entry]): Int = {
    entries.iterator
      .flatMap(_.outputs)
      .count(isUnique)
  }


  def parseEntry(s: String): Entry = {
    val Array(patternsStr, outputsStr) = s.split(" \\| ", 2)
    val patterns = patternsStr.split(" ").toSeq
    val outputs = outputsStr.split(" ").toSeq
    Entry(patterns, outputs)
  }

  def parseEntries(input: String): Seq[Entry] = input.linesIterator.map(parseEntry).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countUniqueOutputs(parseEntries(input)))
  }
}
