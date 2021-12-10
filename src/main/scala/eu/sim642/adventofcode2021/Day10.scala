package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day10 extends RegexParsers {

  sealed trait ParseLineResult[+A]
  case object Legal extends ParseLineResult[Nothing]
  case class Incomplete[+A](expected: A) extends ParseLineResult[A]
  case class Corrupted(actual: Char) extends ParseLineResult[Nothing]

  sealed trait Solution {
    type A

    def parseLine(line: String): ParseLineResult[A]

    private val charErrorScore: Map[Char, Int] = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137,
    )

    def totalSyntaxErrorScore(lines: Seq[String]): Int = {
      lines.flatMap(line => {
        parseLine(line) match {
          case Corrupted(actual) => Some(charErrorScore(actual)) // TODO: why doesn't apply work?
          case _ => None
        }
      }).sum
    }

    def completeLine(line: String, incomplete: Incomplete[A]): String

    // for testing
    def completeLine(line: String): String = parseLine(line) match {
      case incomplete@Incomplete(expected) => completeLine(line, incomplete)
    }

    private val charCompletionScore: Map[Char, Int] = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4,
    )

    def completionScore(completion: String): Long = {
      completion.foldLeft(0L)((acc, c) => 5 * acc + charCompletionScore(c))
    }

    def middleCompletionScore(lines: Seq[String]): Long = {
      val scores = lines.flatMap(line => {
        parseLine(line) match {
          case incomplete@Incomplete(_) => Some(completionScore(completeLine(line, incomplete)))
          case _ => None
        }
      })
      scores.sorted.apply(scores.size / 2)
    }
  }

  // TODO: extract expected char solution
  object ParserCombinatorSolution extends Solution {

    override type A = Char

    private val incompleteMsgRegex = """'(.)' expected but end of source found""".r
    private val corruptedMsgRegex = """'.' expected but '.' found""".r

    override def parseLine(line: String): ParseLineResult[Char] = {

      def chunks: Parser[Unit] = rep(chunk) ^^^ ()

      def chunk: Parser[Unit] = (
        "(" ~> chunks <~ ")"
          | "[" ~> chunks <~ "]"
          | "{" ~> chunks <~ "}"
          | "<" ~> chunks <~ ">"
        )

      parseAll(chunks, line) match {
        case Success(result, next) => Legal
        case Failure(msg, next) => msg match {
          case incompleteMsgRegex(c) => Incomplete(c.charAt(0))
          case corruptedMsgRegex() => Corrupted(line.charAt(next.offset))
        }
        // TODO: Error
      }
    }

    override def completeLine(line: String, incomplete: Incomplete[Char]): String = {

      @tailrec
      def helper(line: String, incomplete: Incomplete[Char], acc: String): String = {
        val newLine = line + incomplete.expected
        val newAcc = acc + incomplete.expected
        parseLine(newLine) match {
          case newIncomplete@Incomplete(_) => helper(newLine, newIncomplete, newAcc)
          case Legal => newAcc
        }
      }

      helper(line, incomplete, "")
    }
  }

  // TODO: extract expected string solution
  object StackSolution extends Solution {

    override type A = String

    private val oppositeChar = Map(
      '(' -> ')',
      '[' -> ']',
      '{' -> '}',
      '<' -> '>',
    )

    override def parseLine(line: String): ParseLineResult[String] = {

      @tailrec
      def helper(line: List[Char], stack: List[Char]): ParseLineResult[String] = line match {
        case Nil => stack match {
          case Nil => Legal
          case _ => Incomplete(stack.mkString)
        }
        case (x@('(' | '[' | '{' | '<')) :: xs => helper(xs, oppositeChar(x) :: stack)
        case x :: xs => stack match {
          case Nil => Corrupted(x)
          case y :: ys if x == y => helper(xs, ys)
          case y :: ys => Corrupted(x)
        }
      }

      helper(line.toList, Nil)
    }

    override def completeLine(line: String, incomplete: Incomplete[String]): String = incomplete.expected
  }


  def parseLines(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import StackSolution._

    println(totalSyntaxErrorScore(parseLines(input)))
    println(middleCompletionScore(parseLines(input)))

    // part 2: 45691823 - wrong (Int overflow, change to Long)
  }
}
