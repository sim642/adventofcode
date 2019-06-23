package eu.sim642.adventofcode2016

object Day4 {

  case class Room(name: String, sectorId: Int, checksum: String)

  def isReal(room: Room): Boolean = {
    val dashlessName = room.name.filter(_ != '-')
    val histogram = dashlessName.groupBy(c => c).mapValues(_.length)
    val mostCommon = histogram.toSeq.sortBy({ case (c, count) => (-count, c)})
    val mostCommon5 = mostCommon.take(5).map(_._1).mkString("")
    mostCommon5 == room.checksum
  }

  def realSectorIdSum(rooms: Seq[Room]): Int = rooms.filter(isReal).map(_.sectorId).sum

  private val roomRegex = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r

  def parseRoom(roomStr: String): Room = roomStr match {
    case roomRegex(name, sectorId, checksum) =>
      Room(name, sectorId.toInt, checksum)
  }

  def parseRooms(input: String): Seq[Room] = input.lines.map(parseRoom).toSeq

  def realSectorIdSum(input: String): Int = realSectorIdSum(parseRooms(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(realSectorIdSum(input))
  }
}
