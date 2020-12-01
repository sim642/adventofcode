package eu.sim642.adventofcode2020

object Day1 {

  sealed trait Solution {
    def entryProduct2020(entries: Seq[Int], n: Int): Int
  }

  object NaiveSolution extends Solution {
    override def entryProduct2020(entries: Seq[Int], n: Int): Int = {
      entries.combinations(n).find(_.sum == 2020).get.product
    }
  }

  object SetContainsSolution extends Solution {
    override def entryProduct2020(entries: Seq[Int], n: Int): Int = {
      val entriesSet = entries.toSet
      val init = entries.combinations(n - 1).find(c => entriesSet.contains(2020 - c.sum)).get
      init.product * (2020 - init.sum)
    }
  }

  def parseEntries(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import SetContainsSolution._

    println(entryProduct2020(parseEntries(input), 2))
    println(entryProduct2020(parseEntries(input), 3))
  }
}
