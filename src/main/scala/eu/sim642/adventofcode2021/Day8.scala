package eu.sim642.adventofcode2021

import scala.collection.immutable.{BitSet, SortedSet}

object Day8 {

  type Signals = BitSet

  def parseSignals(s: String): Signals = s.map(_ - 'a').to(BitSet)

  case class Entry(patterns: Seq[Signals], outputs: Seq[Signals])

  private val uniqueSignalsLengths = Set(2, 3, 4, 7)

  def isUnique(signals: Signals): Boolean = uniqueSignalsLengths.contains(signals.size)

  def countUniqueOutputs(entries: Seq[Entry]): Int = {
    entries.iterator
      .flatMap(_.outputs)
      .count(isUnique)
  }


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


  sealed trait Part2Solution {
    def decodeEntry(entry: Entry): Int

    def sumDecodeEntries(entries: Seq[Entry]): Int = entries.map(decodeEntry).sum
  }

  /**
   * Solution base, which finds a decoding permutation.
   */
  sealed trait PermutationPart2Solution extends Part2Solution {

    type Permutation = IndexedSeq[Int]

    protected def decodeSignals(permutation: Permutation)(signals: Signals): Signals = signals.map(permutation)

    protected def decodePermutation(entry: Entry): Permutation

    override def decodeEntry(entry: Entry): Int = {
      val permutation = decodePermutation(entry)
      val decodedOutputs = entry.outputs.map(decodeSignals(permutation))
      decodedOutputs.map(signalsDigits).foldLeft(0)((acc, digit) => 10 * acc + digit)
    }
  }

  /**
   * Solution, which naively checks all permutations for each entry.
   */
  object NaivePart2Solution extends PermutationPart2Solution {

    override protected def decodePermutation(entry: Entry): Permutation = {
      given Ordering[SortedSet[Int]] = Ordering.Implicits.sortedSetOrdering // for sorted
      val allPatternsSorted = signalsDigits.keys.toSeq.sorted

      (0 to 6).permutations
        .find(permutation => {
          val decodedPatterns = entry.patterns.map(decodeSignals(permutation))
          decodedPatterns.sorted == allPatternsSorted // faster than comparing as Sets
        })
        .get
    }
  }

  /**
   * Solution, which precomputes all permutations and uses inverse permutations for decoding.
   */
  object PrecomputePart2Solution extends PermutationPart2Solution {

    private def invertPermutation(permutation: Permutation): Permutation = {
      permutation.zipWithIndex.sortBy(_._1).map(_._2)
    }

    private lazy val decodeMap: Map[Set[Signals], Permutation] = {
      val allSignals = signalsDigits.keySet

      (0 to 6).permutations
        .map(permutation => {
          allSignals.map(decodeSignals(permutation)) -> invertPermutation(permutation)
        })
        .toMap
    }

    override protected def decodePermutation(entry: Entry): Permutation = decodeMap(entry.patterns.toSet)
  }


  def parseEntry(s: String): Entry = {
    val Array(patternsStr, outputsStr) = s.split(" \\| ", 2)
    val patterns = patternsStr.split(" ").toSeq.map(parseSignals)
    val outputs = outputsStr.split(" ").toSeq.map(parseSignals)
    Entry(patterns, outputs)
  }

  def parseEntries(input: String): Seq[Entry] = input.linesIterator.map(parseEntry).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import PrecomputePart2Solution._

    println(countUniqueOutputs(parseEntries(input)))
    println(sumDecodeEntries(parseEntries(input)))
  }
}
