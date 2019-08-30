package eu.sim642.adventofcode2015

import scala.collection.mutable
import scala.util.parsing.combinator._

object Day7 extends RegexParsers {

  type Value = Int
  private val valueMask = 0xFFFF

  sealed trait Expr

  sealed trait SimpleExpr extends Expr
  case class Const(value: Value) extends SimpleExpr
  case class Wire(ident: String) extends SimpleExpr

  case class Not(expr: SimpleExpr) extends Expr
  case class And(left: SimpleExpr, right: SimpleExpr) extends Expr
  case class Or(left: SimpleExpr, right: SimpleExpr) extends Expr
  case class Lshift(left: SimpleExpr, right: SimpleExpr) extends Expr
  case class Rshift(left: SimpleExpr, right: SimpleExpr) extends Expr


  def eval(instructions: Map[String, Expr])(ident: String): Value = {
    val cache = mutable.Map.empty[String, Value]

    def evalExpr(expr: Expr): Value = expr match {
      case Const(value) => value
      case Wire(ident) =>
        cache.getOrElseUpdate(ident, evalExpr(instructions(ident)))
      case Not(expr) => ~evalExpr(expr) & valueMask
      case And(left, right) => evalExpr(left) & evalExpr(right)
      case Or(left, right) => evalExpr(left) | evalExpr(right)
      case Lshift(left, right) => (evalExpr(left) << evalExpr(right)) & valueMask
      case Rshift(left, right) => evalExpr(left) >> evalExpr(right)
    }

    evalExpr(Wire(ident))
  }

  def evalA(input: String): Value = eval(parseInstructions(input))("a")


  def parseInstruction(s: String): (String, Expr) = {
    def ident: Parser[String] = "[a-z]+".r

    def simpleExpr: Parser[SimpleExpr] = (
      "\\d+".r ^^ { value => Const(value.toInt) }
    | ident ^^ Wire
    )

    def expr: Parser[Expr] = (
      "NOT" ~> simpleExpr ^^ Not
    | simpleExpr ~ "AND" ~ simpleExpr ^^ { case left ~ "AND" ~ right => And(left, right)}
    | simpleExpr ~ "OR" ~ simpleExpr ^^ { case left ~ "OR" ~ right => Or(left, right)}
    | simpleExpr ~ "LSHIFT" ~ simpleExpr ^^ { case left ~ "LSHIFT" ~ right => Lshift(left, right)}
    | simpleExpr ~ "RSHIFT" ~ simpleExpr ^^ { case left ~ "RSHIFT" ~ right => Rshift(left, right)}
    | simpleExpr
    )

    def instruction: Parser[(String, Expr)] = (
      expr ~ "->" ~ ident ^^ { case expr ~ "->" ~ ident => ident -> expr}
    )

    parseAll(instruction, s) match {
      case Success(result, next) => result
      case NoSuccess(msg, next) => throw new RuntimeException(s"Assignment parsing error: $msg ($next)")
    }
  }

  def parseInstructions(input: String): Map[String, Expr] = input.linesIterator.map(parseInstruction).toMap


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(evalA(input))
  }
}
