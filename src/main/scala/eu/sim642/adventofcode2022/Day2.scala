package eu.sim642.adventofcode2022

import Day2.Hand._

object Day2 {

  enum Hand(val score: Int) {
    case Rock extends Hand(1)
    case Paper extends Hand(2)
    case Scissors extends Hand(3)
  }

  type MyChar = 'X' | 'Y' | 'Z'
  type Strategy = Seq[(Hand, MyChar)]

  trait Part {
    def myHand(other: Hand, myChar: MyChar): Hand

    def roundScore(other: Hand, myChar: MyChar): Int = {
      val my = myHand(other, myChar)
      val outcomeScore = (other, my) match {
        case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => 3 // tie
        case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => 6 // my win
        case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => 0 // my loss
      }
      outcomeScore + my.score
    }

    def strategyScore(strategy: Strategy): Int = strategy.view.map(roundScore).sum
  }

  object Part1 extends Part {
    override def myHand(other: Hand, myChar: MyChar): Hand = {
      myChar match {
        case 'X' => Rock
        case 'Y' => Paper
        case 'Z' => Scissors
      }
    }
  }

  object Part2 extends Part {
    override def myHand(other: Hand, myChar: MyChar): Hand = {
      (other, myChar) match {
        case (Rock, 'X') => Scissors // my loss
        case (Paper, 'X') => Rock // my loss
        case (Scissors, 'X') => Paper // my loss
        case (other, 'Y') => other // tie
        case (Rock, 'Z') => Paper // my win
        case (Paper, 'Z') => Scissors // my win
        case (Scissors, 'Z') => Rock // my win
      }
    }
  }

  def parseRound(s: String): (Hand, MyChar) = {
    val Array(otherStr, myStr) = s.split(' ')
    val other = otherStr.charAt(0) match {
      case 'A' => Rock
      case 'B' => Paper
      case 'C' => Scissors
    }
    val my: MyChar = myStr.charAt(0) match { // TODO: simplify
      case 'X' => 'X'
      case 'Y' => 'Y'
      case 'Z' => 'Z'
    }
    (other, my)
  }

  def parseStrategy(input: String): Strategy = input.linesIterator.map(parseRound).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.strategyScore(parseStrategy(input)))
    println(Part2.strategyScore(parseStrategy(input)))
  }
}
