package eu.sim642.adventofcode2020

import scala.util.parsing.combinator.RegexParsers

object Day18 extends RegexParsers {

  sealed trait Expr
  case class Num(value: Int) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Mul(left: Expr, right: Expr) extends Expr

  def eval(expr: Expr): Long = expr match {
    case Num(value) => value
    case Add(left, right) => eval(left) + eval(right)
    case Mul(left, right) => eval(left) * eval(right)
  }

  def sumEvals(exprs: Seq[Expr]): Long = exprs.map(eval).sum

  sealed trait Part {
    def parseExpr(s: String): Expr

    def parseExprs(input: String): Seq[Expr] = input.linesIterator.map(parseExpr).toSeq
  }

  // TODO: reduce duplication

  object Part1 extends Part {

    override def parseExpr(s: String): Expr = {

      def simple: Parser[Expr] = (
        "\\d+".r ^^ { value => Num(value.toInt) }
      | "(" ~> expr <~ ")"
      )

      def op: Parser[(Expr, Expr) => Expr] = (
        "+" ^^^ Add
      | "*" ^^^ Mul
      )

      def expr: Parser[Expr] = chainl1(simple, op)

      parseAll(expr, s) match {
        case Success(result, next) => result
        case noSuccess: NoSuccess => throw new RuntimeException(s"Expr parsing error: ${noSuccess.msg} (${noSuccess.next})")
      }
    }
  }

  object Part2 extends Part {

    override def parseExpr(s: String): Expr = {
      def term: Parser[Expr] = (
        "\\d+".r ^^ { value => Num(value.toInt) }
      | "(" ~> expr <~ ")"
      )

      def factor: Parser[Expr] = chainl1(term, "+" ^^^ Add)

      def expr: Parser[Expr] = chainl1(factor, "*" ^^^ Mul)

      parseAll(expr, s) match {
        case Success(result, next) => result
        case noSuccess: NoSuccess => throw new RuntimeException(s"Expr parsing error: ${noSuccess.msg} (${noSuccess.next})")
      }
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumEvals(Part1.parseExprs(input)))
    println(sumEvals(Part2.parseExprs(input)))

    // part 1: 16948449650 - too low (Int overflowed in eval)
  }
}
