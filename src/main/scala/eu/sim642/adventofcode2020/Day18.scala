package eu.sim642.adventofcode2020

import Day18.Expr._
import scala.util.parsing.combinator.RegexParsers

object Day18 extends RegexParsers {

  enum Expr {
    case Num(value: Int)
    case Add(left: Expr, right: Expr)
    case Mul(left: Expr, right: Expr)
  }

  def eval(expr: Expr): Long = expr match {
    case Num(value) => value
    case Add(left, right) => eval(left) + eval(right)
    case Mul(left, right) => eval(left) * eval(right)
  }

  def sumEvals(exprs: Seq[Expr]): Long = exprs.map(eval).sum


  sealed trait Part {

    def simple: Parser[Expr] = (
      "\\d+".r ^^ { value => Num(value.toInt) }
    | "(" ~> expr <~ ")"
    )

    def expr: Parser[Expr]

    def parseExpr(s: String): Expr = {
      parseAll(expr, s) match {
        case Success(result, next) => result
        case noSuccess: NoSuccess => throw new RuntimeException(s"Expr parsing error: ${noSuccess.msg} (${noSuccess.next})")
      }
    }

    def parseExprs(input: String): Seq[Expr] = input.linesIterator.map(parseExpr).toSeq
  }

  object Part1 extends Part {

    def op: Parser[(Expr, Expr) => Expr] = (
      "+" ^^^ Add.apply _
    | "*" ^^^ Mul.apply _
    )

    def expr: Parser[Expr] = chainl1(simple, op)
  }

  object Part2 extends Part {

    def factor: Parser[Expr] = chainl1(simple, "+" ^^^ Add.apply)

    def expr: Parser[Expr] = chainl1(factor, "*" ^^^ Mul.apply)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumEvals(Part1.parseExprs(input)))
    println(sumEvals(Part2.parseExprs(input)))

    // part 1: 16948449650 - too low (Int overflowed in eval)
  }
}
