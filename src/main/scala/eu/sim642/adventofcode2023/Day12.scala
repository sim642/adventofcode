package eu.sim642.adventofcode2023

import scala.collection.mutable

object Day12 {

  private val countArrangementsMemo = mutable.Map.empty[(List[Char], List[Int]), Long]

  def countArrangements(mask: List[Char], lengths: List[Int]): Long = {
    countArrangementsMemo.getOrElseUpdate((mask, lengths), {
      (mask, lengths) match {
        case (mask, Nil) if mask.contains('#') => 0 // all lengths matched, but # left
        case (_, Nil) => 1 // all lengths matched, only .-s left (possibly as ?)
        case (Nil, _ :: _) => 0 // unmatched lengths
        case ('.' :: newMask, lengths) => countArrangements(newMask, lengths) // skip .
        case ('#' :: newMask, length :: newLengths) if mask.length >= length => // enough length remaining
          val (newMaskPrefix, newMaskSuffix) = newMask.splitAt(length - 1)
          if (newMaskPrefix.contains('.'))
            0 // cannot match all of length
          else {
            newMaskSuffix match {
              case Nil => countArrangements(Nil, newLengths) // matched length at the end
              case ('.' | '?') :: newMask => countArrangements(newMask, newLengths) // matched length (can) stop
              case _ => 0 // # after matched length, but must be delimited by .
            }
          }
        case ('#' :: _, _ :: _) => 0 // not enough length remaining
        case ('?' :: newMask, lengths) => // recursively solve both cases
          countArrangements(newMask, lengths) + countArrangements('#' :: newMask, lengths)
        case (mask, lengths) => throw new IllegalArgumentException(s"impossible mask ($mask) and lengths ($lengths)")
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
