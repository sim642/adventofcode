package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day14 {

  def rollColumn(column: Vector[Char]): Vector[Char] = {

    /*def rollOne(column: Vector[Char]): Vector[Char] = {
      ('#' +: column :+ '.')
        .sliding(3)
        .map({
          case Seq(_  , '#', _  ) => '#'

          case Seq(_, '.', '#') => '.'
          case Seq('#', 'O', _) => 'O'
          case Seq(_, '.', 'O') => 'O'

          case Seq('.', 'O', 'O') => 'O'
          case Seq('O', 'O', 'O') => 'O'

          case Seq(_, '.', '.') => '.'

          case Seq('O', 'O', '.') => 'O'
          case Seq('.', 'O', '.') => '.'
          case Seq('.', 'O', '#') => '.'
          //case Seq('#', 'O', 'O') => 'O'
          //case Seq('O', 'O', '.') => 'O'
          //case Seq('O', '.', 'O') => 'O'
          //case Seq('.', 'O', '.') => '.'
          //case Seq('O', '.', '.') => '.'
          //case Seq('.', '.', '#') => '.'
          //case Seq('.', '#', '#') => '#'
          //case Seq('#', '#', '.') => '#'
          //case Seq('O', 'O', 'O') => 'O'
          //case Seq('.', '.', '.') => '.'
          //case Seq('#', '.', '.') => '.'
          //case Seq('.', '.', 'O') => 'O'
          //case Seq('.', 'O', 'O') => 'O'
          //case Seq('#', '.', 'O') => 'O'
          //case Seq('.', '#', 'O') => '#'
          //case Seq('#', 'O', '.') => 'O'
          //case Seq('O', '.', '#') => '.'
          //case Seq('.', '#', '.') => '#'
          //case Seq('#', '.', '#') => '.'
          //case Seq('.', 'O', '#') => '.'
          //case Seq('O', '#', '.') => '#'
          case x => throw new IllegalArgumentException(x.toString())
        })
        .toVector
    }*/

    def rollOne(column: Vector[Char]): Vector[Char] = {

      def helper(column: List[Char]): List[Char] = column match {
        case Nil => Nil
        case '#' :: Nil => '#' :: Nil
        case '.' :: Nil => '.' :: Nil
        case 'O' :: Nil => 'O' :: Nil
        case 'O' :: 'O' :: newColumn => 'O' :: helper('O' :: newColumn)
        case 'O' :: '.' :: newColumn => 'O' :: helper('.' :: newColumn)
        case '.' :: 'O' :: newColumn => 'O' :: helper('.' :: newColumn)
        case '.' :: '.' :: newColumn => '.' :: helper('.' :: newColumn)
        case '.' :: '#' :: newColumn => '.' :: helper('#' :: newColumn)
        case '#' :: '#' :: newColumn => '#' :: helper('#' :: newColumn)
        case '#' :: 'O' :: newColumn => '#' :: helper('O' :: newColumn)
        case '#' :: '.' :: newColumn => '#' :: helper('.' :: newColumn)
        case 'O' :: '#' :: newColumn => 'O' :: helper('#' :: newColumn)
        case _ => throw new IllegalArgumentException(column.toString())
      }

      helper(column.toList).toVector
    }

    NaiveCycleFinder.find(column, rollOne).cycleHead
  }

  def totalLoad(grid: Grid[Char]): Int = {
    grid.transpose
      .map(rollColumn)
      .tapEach(println)
      .map(_.reverse.zipWithIndex.map({
        case ('O', i) => i + 1
        case _ => 0
      }).sum)
      .sum
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalLoad(parseGrid(input)))
  }
}
