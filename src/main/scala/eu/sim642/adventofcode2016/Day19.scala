package eu.sim642.adventofcode2016

object Day19 {

  // 1 2 3 4 5
  // 1 1 1 1 1
  // 2   2   .
  //     2   3
  //     5

  // 1 2 3 4
  // 1 1 1 1
  // 2   2
  // 4

  // 1 2 3 4 5 6 7
  // 1 1 1 1 1 1 1
  // 2   2   2   .
  //     2   2   3
  //     4       .
  //             7

  def lastElf(elfCount: Int): Int = {

    /*def helper(elfCount: Int, firstElf: Int, gapSize: Int): Int = {
      if (elfCount == 1)
        firstElf
      else if (elfCount % 2 == 0)
        helper(elfCount / 2, firstElf, gapSize * 2)
      else
        helper(elfCount / 2, firstElf + gapSize * 2, gapSize * 2)
    }

    helper(elfCount, 1, 1)*/

    //2 * (elfCount - Integer.highestOneBit(elfCount)) + 1
    (elfCount ^ Integer.highestOneBit(elfCount)) << 1 | 1
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim
  val input = 3014603

  def main(args: Array[String]): Unit = {
    println(lastElf(input))
  }
}
