package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day10 extends RegexParsers {

  sealed trait ParseLineResult[+A]
  case object Legal extends ParseLineResult[Nothing]
  case class Incomplete[+A](expected: A) extends ParseLineResult[A]
  case class Corrupted(actual: Char) extends ParseLineResult[Nothing]

  private val charErrorScore: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  private val charCompletionScore: Map[Char, Int] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4,
  )

  def completionScore(completion: String): Long = {
    completion.foldLeft(0L)((acc, c) => 5 * acc + charCompletionScore(c))
  }

  sealed trait Solution {
    type A

    def parseLine(line: String): ParseLineResult[A]

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

  trait ExpectedCharSolution extends Solution {
    override type A = Char

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

  object ParserCombinatorSolution extends ExpectedCharSolution {

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
  }

  trait ExpectedStringSolution extends Solution {
    override type A = String

    override def completeLine(line: String, incomplete: Incomplete[String]): String = incomplete.expected
  }

  object StackSolution extends ExpectedStringSolution {

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
  }

  object RecursiveDescentSolution extends ExpectedStringSolution {

    // TODO: deduplicate
    private val oppositeChar = Map(
      '(' -> ')',
      '[' -> ']',
      '{' -> '}',
      '<' -> '>',
    )

    override def parseLine(line: String): ParseLineResult[String] = {

      @tailrec
      def chunks(line: List[Char]): (ParseLineResult[String], List[Char]) = line match {
        case ('(' | '[' | '{' | '<') :: _ =>
          val resultLine2@(result, line2) = chunk(line)
          result match {
            case Legal => chunks(line2)
            case _ => resultLine2
          }
        case _ => (Legal, line)
      }

      def chunk(line: List[Char]): (ParseLineResult[String], List[Char]) = line match {
        case (x@('(' | '[' | '{' | '<')) :: line2 =>
          val (result, line3) = chunks(line2)
          val oppositeX = oppositeChar(x)
          result match {
            case Legal =>
              line3 match {
                case Nil => (Incomplete(oppositeX.toString), Nil)
                case y :: line4 if y == oppositeX => (Legal, line4)
                case y :: line4 => (Corrupted(y), line4)
              }
            case Incomplete(expected) => (Incomplete(expected + oppositeX), line3)
            case corrupted@Corrupted(_) => (corrupted, line3)
          }
        case _ => (Legal, line)
      }

      chunks(line.toList)._1
    }
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
