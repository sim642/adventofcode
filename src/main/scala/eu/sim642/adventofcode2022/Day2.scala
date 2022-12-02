package eu.sim642.adventofcode2022

import Day2.Hand._

object Day2 {

  enum Hand(val score: Int) {
    case Rock extends Hand(1)
    case Paper extends Hand(2)
    case Scissors extends Hand(3)
  }

  type Strategy = Seq[(Hand, Hand)]

  def roundScore(other: Hand, my: Hand): Int = {
    val outcomeScore = (other, my) match {
      case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3 // tie
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 6 // my win
      case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => 0 // my loss
    }
    outcomeScore + my.score
  }

  def strategyScore(strategy: Strategy): Int = strategy.view.map(roundScore).sum

  def parseRound(s: String): (Hand, Hand) = {
    val Array(otherStr, myStr) = s.split(' ')
    val other = otherStr.charAt(0) match {
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors
    }
    val my = myStr.charAt(0) match {
      case 'X' => Rock
      case 'Y' => Paper
      case 'Z' => Scissors
    }
    (other, my)
  }

  def parseStrategy(input: String): Strategy = input.linesIterator.map(parseRound).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strategyScore(parseStrategy(input)))
  }
}
