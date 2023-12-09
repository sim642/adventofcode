package eu.sim642.adventofcode2016

import scala.collection.mutable

object Day19 {

  trait Part {
    def lastElf(elfCount: Int): Int
  }

  object Part1 extends Part {
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

    override def lastElf(elfCount: Int): Int = {
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
  }

  object Part2 extends Part {
    // 1 2 3 4 5

    // 1 1 1 1 1
    // 2 1   1 1
    // 2 2   1
    //   2   3

    //   5

    // 1 2 3 4 5 6 7
    // 1 2 3   5 6 7
    // 1 2 3   5   7
    // 1 2 3   5
    // 1   3   5
    // 1       5
    //         5


    // 1 2 3 4 5 6 7 8 9
    // 1 2 3 4   6 7 8 9
    // 1 2 3 4   6   8 9
    // 1 2 3 4   6     9
    //   2 3 4   6     9
    //   2 3     6     9
    //   2 3     6
    //     3     6
    //     3

    override def lastElf(elfCount: Int): Int = {
      // https://old.reddit.com/r/adventofcode/comments/5j4lp1/2016_day_19_solutions/dbdj8jh/
      // mutable.Queue because immutable.Queue is too slow
      val left = (1 to elfCount / 2).to(mutable.Queue)
      val right = (elfCount / 2 + 1 to elfCount).to(mutable.Queue)

      while (left.size + right.size > 1) {
        right.dequeue() // discard because gifts taken
        right.enqueue(left.dequeue()) // rotate current to end
        if ((left.size + right.size) % 2 == 0)
          left.enqueue(right.dequeue()) // some get skipped when taking gifts
      }

      right.dequeue()
    }
  }

  //lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim
  val input = 3014603

  def main(args: Array[String]): Unit = {
    println(Part1.lastElf(input))
    println(Part2.lastElf(input))
  }
}
