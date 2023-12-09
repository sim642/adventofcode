package eu.sim642.adventofcode2023

object Day9 {

  type History = Seq[Int]

  def difference(history: History): History = (history.tail lazyZip history).map(_ - _)

  trait Part {
    def extrapolate(history: History): Int

    def sumExtrapolated(histories: Seq[History]): Int = histories.map(extrapolate).sum
  }

  object Part1 extends Part {
    override def extrapolate(history: History): Int = {
      LazyList.iterate(history)(difference)
        .takeWhile(!_.forall(_ == 0))
        .map(_.last)
        .sum
    }
  }

  object Part2 extends Part {
    override def extrapolate(history: History): Int = Part1.extrapolate(history.reverse)
  }


  def parseHistories(input: String): Seq[History] =
    input.linesIterator.map(_.split(' ').map(_.toInt).toSeq).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumExtrapolated(parseHistories(input)))
    println(Part2.sumExtrapolated(parseHistories(input)))
  }
}
