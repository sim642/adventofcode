package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid

object Day4 {

  case class Input(numbers: Seq[Int], boards: Seq[Grid[Int]])

  case class Bingo(rows: Seq[Set[Int]], cols: Seq[Set[Int]]) {
    def mark(n: Int): Bingo = {
      val newRows = rows.map(_ - n)
      val newCols = cols.map(_ - n)
      Bingo(newRows, newCols)
    }

    def isWon: Boolean = rows.exists(_.isEmpty) || cols.exists(_.isEmpty)

    def sumUnmarked: Int = rows.map(_.sum).sum
  }

  def board2bingo(board: Grid[Int]): Bingo = {
    val rows = board.map(_.toSet)
    val cols = board.transpose.map(_.toSet)
    Bingo(rows, cols)
  }

  def findWin(bingo: Bingo, numbers: Seq[Int]): (Int, Bingo) = {
    val (winBoard, i) = numbers.iterator.scanLeft(bingo)(_.mark(_)).zipWithIndex.find(_._1.isWon).get
    (i - 1, winBoard) // off by one because scanLeft includes initial
  }

  sealed trait Part {
    protected val iOrd: Ordering[Int]

    def findMinWin(input: Input): (Int, Bingo) = {
      val Input(numbers, boards) = input
      boards.map(board2bingo).map(findWin(_, numbers)).minBy(_._1)(iOrd)
    }

    def winScore(input: Input): Int = {
      val (i, bingo) = findMinWin(input)
      bingo.sumUnmarked * input.numbers(i)
    }
  }

  object Part1 extends Part {
    override protected val iOrd: Ordering[Int] = Ordering.Int
  }

  object Part2 extends Part {
    override protected val iOrd: Ordering[Int] = Ordering.Int.reverse
  }


  def parseBoard(s: String): Grid[Int] = s.linesIterator.map(_.trim.split(" +").toVector.map(_.toInt)).toVector // trim because split keeps initial empty

  def parseInput(input: String): Input = {
    val numbersStr +: boardStrs = input.split("\n\n").toSeq
    val numbers = numbersStr.split(",").toSeq.map(_.toInt)
    val boards = boardStrs.map(parseBoard)
    Input(numbers, boards)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.winScore(parseInput(input)))
    println(Part2.winScore(parseInput(input)))
  }
}
