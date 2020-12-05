package eu.sim642.adventofcode2020

object Day5 {

  case class Seat(row: Int, column: Int) {
    def seatId: Int = 8 * row + column
  }

  def highestSeatId(seats: Seq[Seat]): Int = seats.map(_.seatId).max

  def missingSeatId(seats: Seq[Seat]): Int = {
    val seatIds = seats.map(_.seatId).toSet
    val allSeatIds = (seatIds.min to seatIds.max).toSet
    val missingSeatIds = allSeatIds -- seatIds
    assert(missingSeatIds.size == 1)
    missingSeatIds.head
  }


  def parseSeat(s: String): Seat = {
    val (row, column) = s.splitAt(7)
    Seat(
      Integer.parseInt(row.replace('F', '0').replace('B', '1'), 2),
      Integer.parseInt(column.replace('L', '0').replace('R', '1'), 2),
    )
  }

  def parseSeats(input: String): Seq[Seat] = input.linesIterator.map(parseSeat).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(highestSeatId(parseSeats(input)))
    println(missingSeatId(parseSeats(input)))
  }
}
