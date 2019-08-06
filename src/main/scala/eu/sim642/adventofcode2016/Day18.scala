package eu.sim642.adventofcode2016

object Day18 {

  def nextRow(row: String): String = {
    ("." + row + ".").sliding(3).map({
      case "^^." | ".^^" | "^.." | "..^" => '^'
      case _ => '.'
    }).mkString
  }

  def rows(startRow: String): Iterator[String] = Iterator.iterate(startRow)(nextRow)

  def countSafe(startRow: String, totalRows: Int = 40): Int = {
    rows(startRow).take(totalRows).map(_.count(_ == '.')).sum
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSafe(input))
  }
}
