package eu.sim642.adventofcode2020

import scala.collection.immutable.SortedSet
import eu.sim642.adventofcodelib.StringImplicits._

object Day5 {

  case class Seat(row: Int, column: Int) {
    def seatId: Int = 8 * row + column
  }

  def highestSeatId(seats: Seq[Seat]): Int = seats.map(_.seatId).max

  def missingSeatId(seats: Seq[Seat]): Int = {
    val seatIds = seats.map(_.seatId).to(SortedSet)
    (seatIds.min to seatIds.max).find(!seatIds.contains(_)).get
  }


  def parseBinary(zero: Char, one: Char)(s: String): Int =
    s.replace(zero, '0').replace(one, '1').toIntRadix(2)

  def parseSeat(s: String): Seat = {
    val (row, column) = s.splitAt(7)
    Seat(
      parseBinary('F', 'B')(row),
      parseBinary('L', 'R')(column)
    )
  }

  def parseSeats(input: String): Seq[Seat] = input.linesIterator.map(parseSeat).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(highestSeatId(parseSeats(input)))
    println(missingSeatId(parseSeats(input)))
  }
}
