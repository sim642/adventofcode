package eu.sim642.adventofcode2023

object Day9 {

  type History = Seq[Int]

  def nextValue(history: History): Int = {
    val differences = LazyList.iterate(history)(history =>
      (history lazyZip history.tail).map(-_ + _)
    )
    val nonZeroDifferences = differences.takeWhile(!_.forall(_ == 0))
    nonZeroDifferences
      .map(_.last)
      .sum
  }

  def sumNextValues(histories: Seq[History]): Int = histories.map(nextValue).sum

  def prevValue(history: History): Int = {
    // TODO: deduplicate
    val differences = LazyList.iterate(history)(history =>
      (history lazyZip history.tail).map(-_ + _)
    )
    val nonZeroDifferences = differences.takeWhile(!_.forall(_ == 0))
    nonZeroDifferences
      .map(_.head)
      .foldRight(0)((a, b) => a - b)
  }

  def sumPrevValues(histories: Seq[History]): Int = histories.map(prevValue).sum


  def parseHistories(input: String): Seq[History] =
    input.linesIterator.map(_.split(' ').map(_.toInt).toSeq).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumNextValues(parseHistories(input)))
    println(sumPrevValues(parseHistories(input)))
  }
}
