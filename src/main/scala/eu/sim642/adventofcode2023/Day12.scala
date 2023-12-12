package eu.sim642.adventofcode2023

import dk.brics.automaton.{Automaton, RegExp}


object Day12 {

  case class Record(mask: String, lengths: Seq[Int]) {

    lazy val maskAutomaton: Automaton = {
      mask.map({
        case '.' => Automaton.makeChar('.')
        case '#' => Automaton.makeChar('#')
        case '?' => Automaton.makeCharSet(".#")
      }).reduce(_ concatenate _)
    }

    lazy val lengthsAutomaton: Automaton = {
      val pad1 = Automaton.makeChar('.').repeat(1)
      val pad0 = Automaton.makeChar('.').repeat(0)
      val ret = pad0 concatenate lengths.map(l =>
        Automaton.makeChar('#').repeat(l, l)
      ).reduce((a, b) => a concatenate pad1 concatenate b) concatenate pad0
      ret.minimize()
      ret
    }

    def possibleArrangements: Int = {
      val strings = (maskAutomaton intersection lengthsAutomaton).getFiniteStrings
      //println(s"$mask $lengths: $strings")
      //println(lengthsAutomaton)
      //println(lengthsAutomaton.run("#.#.###"))
      strings.size()
    }
  }

  def sumPossibleArrangements(records: Seq[Record]): Int = records.map(_.possibleArrangements).sum


  def parseRecord(s: String): Record = s match {
    case s"$mask $lengths" =>
      Record(mask, lengths.split(',').map(_.toInt).toSeq)
  }

  def parseRecords(input: String): Seq[Record] = input.linesIterator.map(parseRecord).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPossibleArrangements(parseRecords(input)))
  }
}
