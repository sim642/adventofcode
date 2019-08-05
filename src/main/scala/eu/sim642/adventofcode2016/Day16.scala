package eu.sim642.adventofcode2016

object Day16 {

  def generateData(initial: String, length: Int): String = {
    val it = Iterator.iterate(initial)(s => s + "0" + s.reverseMap({ case '0' => '1' case '1' => '0' }))
    it.find(_.length >= length).get.take(length)
  }

  def checksum(data: String): String = {
    val newData = data.grouped(2).map({
      case "00" | "11" => '1'
      case "01" | "10" => '0'
    }).mkString

    if (newData.length % 2 == 0)
      checksum(newData)
    else
      newData
  }

  def fillChecksum(initial: String, length: Int = 272): String = checksum(generateData(initial, length))

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim
  val input = "00101000101111010"

  def main(args: Array[String]): Unit = {
    println(fillChecksum(input))
  }
}
