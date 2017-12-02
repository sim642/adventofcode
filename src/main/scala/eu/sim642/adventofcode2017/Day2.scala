package eu.sim642.adventofcode2017

object Day2 {

  trait Part {
    def rowChecksum(row: Seq[Int]): Int

    def checksum(spreadsheet: Seq[Seq[Int]]): Int = spreadsheet.map(rowChecksum).sum

    def checksum(s: String): Int = {
      val spreadsheet = s.trim.split('\n').toSeq.map(_.trim.split("\\s+").toSeq.map(_.toInt))
      checksum(spreadsheet)
    }
  }

  object Part1 extends Part {
    override def rowChecksum(row: Seq[Int]): Int = row.max - row.min
  }

  object Part2 extends Part {
    override def rowChecksum(row: Seq[Int]): Int = {
      val sortedRow = row.sorted.reverse
      val dividePairs = for {
        i <- 0 until sortedRow.size
        j <- (i + 1) until sortedRow.size
        p = sortedRow(i)
        q = sortedRow(j)
        if p % q == 0
      } yield (p, q)

      require(dividePairs.size == 1)
      val (p, q) = dividePairs.head
      p / q
    }
  }

  val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.checksum(input))
    println(Part2.checksum(input))
  }
}
