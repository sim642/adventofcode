package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day4 {

  case class Input(numbers: Seq[Int], boards: Seq[Grid[Int]])

  sealed trait Solution {
    protected def makeFindWin(numbers: Seq[Int]): Grid[Int] => (Int, Int)

    private def winScore(input: Input)(using Ordering[Int]): Int = {
      val Input(numbers, boards) = input
      val findWin = makeFindWin(numbers)
      val (i, sumUnmarked) = boards.map(findWin).minBy(_._1)
      sumUnmarked * input.numbers(i)
    }

    def firstWinScore(input: Input): Int = winScore(input)(using Ordering.Int)

    def lastWinScore(input: Input): Int = winScore(input)(using Ordering.Int.reverse)
  }

  /**
   * Solution, which does marking by removing from row and column sets.
   */
  object SetSolution extends Solution {

    private case class Bingo(rows: Seq[Set[Int]], cols: Seq[Set[Int]]) {
      def mark(n: Int): Bingo = {
        val newRows = rows.map(_ - n)
        val newCols = cols.map(_ - n)
        Bingo(newRows, newCols)
      }

      def isWon: Boolean = rows.exists(_.isEmpty) || cols.exists(_.isEmpty)

      def sumUnmarked: Int = rows.map(_.sum).sum
    }

    private def board2bingo(board: Grid[Int]): Bingo = {
      val rows = board.map(_.toSet)
      val cols = board.transpose.map(_.toSet)
      Bingo(rows, cols)
    }

    override protected def makeFindWin(numbers: Seq[Int]): Grid[Int] => (Int, Int) = {

      def findWin(board: Grid[Int]): (Int, Int) = {
        val initialBingo = board2bingo(board)
        val (winBingo, i) = numbers.iterator.scanLeft(initialBingo)(_.mark(_)).zipWithIndex.find(_._1.isWon).get
        (i - 1, winBingo.sumUnmarked) // off by one because scanLeft includes initial
      }

      findWin
    }
  }

  /**
   * Solution, which finds marking indices for all squares.
   */
  object IndexSolution extends Solution {
    override protected def makeFindWin(numbers: Seq[Int]): Grid[Int] => (Int, Int) = {
      val number2i = numbers.zipWithIndex.toMap // invert numbers only once

      def findWin(board: Grid[Int]): (Int, Int) = {
        val iBoard = board.mapGrid(number2i)
        val iRows = iBoard.map(_.max)
        val iCols = iBoard.transpose.map(_.max)
        val i = iRows.min min iCols.min
        val markedBoard = board.mapGrid(x => if (number2i(x) <= i) 0 else x) // zero out marked numbers
        (i, markedBoard.sumGrid)
      }

      findWin
    }
  }


  def parseBoard(s: String): Grid[Int] = s.linesIterator.map(_.trim.split(" +").toVector.map(_.toInt)).toVector // trim because split keeps initial empty

  def parseInput(input: String): Input = {
    val numbersStr +: boardStrs = input.split("\n\n").toSeq: @unchecked
    val numbers = numbersStr.split(",").toSeq.map(_.toInt)
    val boards = boardStrs.map(parseBoard)
    Input(numbers, boards)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import IndexSolution._

    println(firstWinScore(parseInput(input)))
    println(lastWinScore(parseInput(input)))
  }
}
