package eu.sim642.adventofcode2023

import scala.collection.mutable

object Day12 {

  private val countArrangementsMemo = mutable.Map.empty[(List[Char], List[Int]), Long]

  def countArrangements(mask: List[Char], lengths: List[Int]): Long = {
    countArrangementsMemo.getOrElseUpdate((mask, lengths), {
      (mask, lengths) match {
        case (mask, Nil) if mask.forall(c => c == '.' || c == '?') => 1
        case (_, Nil) => 0
        case (Nil, _) => 0
        case ('.' :: newMask, lengths) => countArrangements(newMask, lengths)
        case ('#' :: newMask, l :: newLengths) =>
          newMask.splitAt(l - 1) match {
            case (a, x :: b) if a.length == l - 1 && a.forall(c => c == '#' || c == '?') && (x == '.' || x == '?') =>
              countArrangements(b, newLengths)
            case (a, Nil) if a.length == l - 1 && a.forall(c => c == '#' || c == '?') =>
              countArrangements(Nil, newLengths)
            case (_, _) => 0
          }
        case ('?' :: newMask, lengths) =>
          countArrangements('.' :: newMask, lengths) + countArrangements('#' :: newMask, lengths)
        case (mask, lengths) => throw IllegalArgumentException(s"$mask $lengths")
      }
    })
  }

  case class Record(mask: String, lengths: Seq[Int]) {

    def five: Record =
      Record(Seq.fill(5)(mask).reduce(_ ++ "?" ++ _), Seq.fill(5)(lengths).reduce(_ ++ _))

    def possibleArrangements: Long = countArrangements(mask.toList, lengths.toList)
  }

  trait Part {
    def possibleArrangements(record: Record): Long

    def sumPossibleArrangements(records: Seq[Record]): Long =
      records.map(possibleArrangements).sum
  }

  object Part1 extends Part {
    override def possibleArrangements(record: Record): Long = record.possibleArrangements
  }

  object Part2 extends Part {
    override def possibleArrangements(record: Record): Long = record.five.possibleArrangements
  }


  def parseRecord(s: String): Record = s match {
    case s"$mask $lengths" =>
      Record(mask, lengths.split(',').map(_.toInt).toSeq)
  }

  def parseRecords(input: String): Seq[Record] = input.linesIterator.map(parseRecord).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumPossibleArrangements(parseRecords(input)))
    println(Part2.sumPossibleArrangements(parseRecords(input)))
  }
}
