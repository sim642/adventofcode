package eu.sim642.adventofcode2016

object Day4 {

  case class Room(name: String, sectorId: Int, checksum: String)

  def isReal(room: Room): Boolean = {
    val dashlessName = room.name.filter(_ != '-')
    val histogram = dashlessName.toSeq.groupBy(c => c).view.mapValues(_.length)
    val mostCommon = histogram.toSeq.sortBy({ case (c, count) => (-count, c)})
    val mostCommon5 = mostCommon.take(5).map(_._1).mkString("")
    mostCommon5 == room.checksum
  }

  def realSectorIdSum(rooms: Seq[Room]): Int = rooms.filter(isReal).map(_.sectorId).sum

  def decrypt(name: String, sectorId: Int): String = {
    name.map({
      case '-' => ' '
      case c => ('a' + (((c - 'a') + sectorId) % ('z' - 'a' + 1))).toChar
    }).mkString("")
  }

  def northPoleObjectsSectorId(rooms: Seq[Room]): Int = {
    rooms.find(room => decrypt(room.name, room.sectorId) == "northpole object storage").get.sectorId
  }

  private val roomRegex = """([a-z\-]+)-(\d+)\[([a-z]+)\]""".r

  def parseRoom(roomStr: String): Room = roomStr match {
    case roomRegex(name, sectorId, checksum) =>
      Room(name, sectorId.toInt, checksum)
  }

  def parseRooms(input: String): Seq[Room] = input.linesIterator.map(parseRoom).toSeq

  def realSectorIdSum(input: String): Int = realSectorIdSum(parseRooms(input))

  def northPoleObjectsSectorId(input: String): Int = northPoleObjectsSectorId(parseRooms(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(realSectorIdSum(input))
    println(northPoleObjectsSectorId(input))
  }
}
