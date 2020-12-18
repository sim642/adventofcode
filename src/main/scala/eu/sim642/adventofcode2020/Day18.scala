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


  def parseExpr(s: String): Expr = {

    def op: Parser[(Expr, Expr) => Expr] = (
      "+" ^^^ Add
    | "*" ^^^ Mul
    )

    def simpleExpr: Parser[Expr] = (
      "\\d+".r ^^ { value => Num(value.toInt) }
    | "(" ~> expr <~ ")"
    )

    def expr: Parser[Expr] = (
      chainl1(simpleExpr, op)
    )

    parseAll(expr, s) match {
      case Success(result, next) => result
      case noSuccess: NoSuccess => throw new RuntimeException(s"Expr parsing error: ${noSuccess.msg} (${noSuccess.next})")
    }
  }

  def parseExprs(input: String): Seq[Expr] = input.linesIterator.map(parseExpr).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumEvals(parseExprs(input)))

    // part 1: 16948449650 - too low (Int overflowed in eval)
  }
}
