package eu.sim642.adventofcode2015

object Day16 {

  type Detection = Map[String, Int]

  private val wrappingDetection: Detection = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1,
  )

  def findWrappingSue(sues: Seq[Detection]): Int = {
    sues.indexWhere(_.forall({ case (compound, count) => wrappingDetection(compound) == count })) + 1
  }

  def findWrappingSue(input: String): Int = findWrappingSue(parseSues(input))


  private val sueRegex = """Sue (\d+): (.*)""".r
  private val detectionRegex = """(\w+): (\d+)""".r

  def parseSue(s: String): Detection = s match {
    case sueRegex(num, detection) =>
      detectionRegex.findAllMatchIn(detection)
        .map(m => m.group(1) -> m.group(2).toInt)
        .toMap
  }

  def parseSues(input: String): Seq[Detection] = input.linesIterator.map(parseSue).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findWrappingSue(input))
  }
}
