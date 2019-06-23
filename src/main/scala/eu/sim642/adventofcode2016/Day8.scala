package eu.sim642.adventofcode2016

object Day8 {

  sealed trait Operation
  case class Rect(a: Int, b: Int) extends Operation
  case class RotateRow(y: Int, by: Int) extends Operation
  case class RotateColumn(x: Int, by: Int) extends Operation

  def litPixels(operations: Seq[Operation]): Int = {
    // assumes no pixel is overdrawn
    operations.map({
      case Rect(a, b) => a * b
      case _ => 0
    }).sum
  }

  private val rectRegex = """rect (\d+)x(\d+)""".r
  private val rotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
  private val rotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r

  def parseOperation(s: String): Operation = s match {
    case rectRegex(a, b) => Rect(a.toInt, b.toInt)
    case rotateRowRegex(y, by) => RotateRow(y.toInt, by.toInt)
    case rotateColumnRegex(x, by) => RotateColumn(x.toInt, by.toInt)
  }

  def parseOperations(input: String): Seq[Operation] = input.lines.map(parseOperation).toSeq

  def litPixels(input: String): Int = litPixels(parseOperations(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(litPixels(input))
  }
}
