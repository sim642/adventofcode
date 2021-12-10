package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object Day10 extends RegexParsers {

  sealed trait ParseLineResult
  case object Legal extends ParseLineResult
  case class Incomplete(expected: Char) extends ParseLineResult
  case class Corrupted(i: Int) extends ParseLineResult

  private val incompleteMsgRegex = """'(.)' expected but end of source found""".r
  private val corruptedMsgRegex = """'.' expected but '.' found""".r

  def parseLine(line: String): ParseLineResult = {

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
        case corruptedMsgRegex() => Corrupted(next.offset)
      }
      // TODO: Error
    }
  }


  private val charErrorScore: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )

  def totalSyntaxErrorScore(lines: Seq[String]): Int = {
    lines.flatMap(line => {
      parseLine(line) match {
        case Corrupted(i) => Some(charErrorScore(line.charAt(i))) // TODO: why doesn't apply work?
        case _ => None
      }
    }).sum
  }

  @tailrec
  def completeLine(line: String, acc: String = ""): String = {
    parseLine(line) match {
      case Incomplete(c) => completeLine(line + c, acc + c)
      case Legal => acc
    }
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
        case Incomplete(_) => Some(completionScore(completeLine(line)))
        case _ => None
      }
    })
    scores.sorted.apply(scores.size / 2)
  }


  def parseLines(input: String): Seq[String] = input.linesIterator.toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalSyntaxErrorScore(parseLines(input)))
    println(middleCompletionScore(parseLines(input)))

    // part 2: 45691823 - wrong (Int overflow, change to Long)
  }
}
