package eu.sim642.adventofcode2016

object Day18 {

  type Row = BigInt // BigInt has bitwise operations, BitSet doesn't

  def nextRow(row: Row, mask: Row): Row = {
    ((row >> 1) ^ (row << 1)) & mask
  }

  def rows(startRow: Row, mask: Row): Iterator[Row] = Iterator.iterate(startRow)(nextRow(_, mask))

  // for backwards-compatible testing
  def rowStrings(input: String): Iterator[String] = {
    val width = input.length
    val mask = (BigInt(1) << width) - 1
    rows(parseInput(input), mask)
      .map({ row =>
        (0 until width)
          .map(bit => row.testBit(bit))
          .map(if (_) '^' else '.')
          .mkString
      })
  }

  trait Part {
    val defaultTotalRows: Int

    def countSafe(width: Int, startRow: Row, totalRows: Int): Int = {
      val mask = (BigInt(1) << width) - 1
      rows(startRow, mask).take(totalRows).map(width - _.bitCount).sum
    }

    def countSafe(input: String, totalRows: Int = defaultTotalRows): Int = {
      countSafe(input.length, parseInput(input), totalRows)
    }
  }

  object Part1 extends Part {
    override val defaultTotalRows: Int = 40
  }

  object Part2 extends Part {
    override val defaultTotalRows: Int = 400000
  }

  def parseInput(input: String): Row = {
    input.view.zipWithIndex
      .filter(_._1 == '^')
      .map(_._2)
      .foldLeft(BigInt(0))((acc, bit) => acc.setBit(bit))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countSafe(input))
    println(Part2.countSafe(input))
  }
}
