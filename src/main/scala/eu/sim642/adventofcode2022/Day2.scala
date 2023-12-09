package eu.sim642.adventofcode2022

object Day2 {

  sealed trait Hand {
    val wins: Hand
    val loses: Hand
    val score: Int
  }

  object Rock extends Hand {
    override val wins: Hand = Scissors
    override val loses: Hand = Paper
    override val score: Int = 1
  }

  object Paper extends Hand {
    override val wins: Hand = Rock
    override val loses: Hand = Scissors
    override val score: Int = 2
  }

  object Scissors extends Hand {
    override val wins: Hand = Paper
    override val loses: Hand = Rock
    override val score: Int = 3
  }

  type MyChar = 'X' | 'Y' | 'Z'
  type Strategy = Seq[(Hand, MyChar)]

  trait Part {
    def myHand(other: Hand, myChar: MyChar): Hand

    def roundScore(other: Hand, myChar: MyChar): Int = {
      val my = myHand(other, myChar)
      val outcomeScore = {
        if (other == my) // draw
          3
        else if (other == my.wins) // my win
          6
        else if (other == my.loses) // my loss
          0
        else
          throw new IllegalStateException("impossible hand combination")
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
      myChar match {
        case 'X' => other.wins // my loss
        case 'Y' => other // draw
        case 'Z' => other.loses // my win
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
    val my: MyChar = myStr.charAt(0) match {
      case myChar@('X' | 'Y' | 'Z') => myChar
    }
    (other, my)
  }

  def parseStrategy(input: String): Strategy = input.linesIterator.map(parseRound).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.strategyScore(parseStrategy(input)))
    println(Part2.strategyScore(parseStrategy(input)))
  }
}
