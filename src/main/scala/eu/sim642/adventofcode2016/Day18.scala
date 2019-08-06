package eu.sim642.adventofcode2016

object Day18 {

  // TODO: optimize with bitset?

  def nextRow(row: String): String = {
    ("." + row + ".").sliding(3).map({
      case "^^." | ".^^" | "^.." | "..^" => '^'
      case _ => '.'
    }).mkString
  }

  def rows(startRow: String): Iterator[String] = Iterator.iterate(startRow)(nextRow)

  trait Part {
    val defaultTotalRows: Int

    def countSafe(startRow: String, totalRows: Int = defaultTotalRows): Int = {
      rows(startRow).take(totalRows).map(_.count(_ == '.')).sum
    }
  }

  object Part1 extends Part {
    override val defaultTotalRows: Int = 40
  }

  object Part2 extends Part {
    override val defaultTotalRows: Int = 400000
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countSafe(input))
    println(Part2.countSafe(input))
  }
}
