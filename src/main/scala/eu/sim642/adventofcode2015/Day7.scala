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


  def eval(instructions: Map[String, Expr]): String => Value = {
    val cache = mutable.Map.empty[String, Value]
    def evalIdent(ident: String): Value = {
      cache.getOrElseUpdate(ident, evalExpr(instructions(ident)))
    }

    def evalExpr(expr: Expr): Value = expr match {
      case Const(value) => value
      case Wire(ident) => evalIdent(ident)
      case Not(expr) => ~evalExpr(expr) & valueMask
      case And(left, right) => evalExpr(left) & evalExpr(right)
      case Or(left, right) => evalExpr(left) | evalExpr(right)
      case Lshift(left, right) => (evalExpr(left) << evalExpr(right)) & valueMask
      case Rshift(left, right) => evalExpr(left) >> evalExpr(right)
    }

    evalIdent
  }

  trait Part {
    def evalA(instructions: Map[String, Expr]): Value

    def evalA(input: String): Value = evalA(parseInstructions(input))
  }

  object Part1 extends Part {
    override def evalA(instructions: Map[String, Expr]): Value = eval(instructions)("a")
  }

  object Part2 extends Part {
    override def evalA(instructions: Map[String, Expr]): Value = {
      val newB = eval(instructions)("a")
      val newInstructions = instructions + ("b" -> Const(newB))
      eval(newInstructions)("a")
    }
  }


  def parseInstruction(s: String): (String, Expr) = {
    // parser combinators might be a bit overkill for this...
    def ident: Parser[String] = "[a-z]+".r

    def simpleExpr: Parser[SimpleExpr] = (
      "\\d+".r ^^ { value => Const(value.toInt) }
    | ident ^^ Wire
    )

    def expr: Parser[Expr] = (
      "NOT" ~> simpleExpr ^^ Not
    | simpleExpr ~ "AND" ~ simpleExpr ^^ { case left ~ _ ~ right => And(left, right)}
    | simpleExpr ~ "OR" ~ simpleExpr ^^ { case left ~ _ ~ right => Or(left, right)}
    | simpleExpr ~ "LSHIFT" ~ simpleExpr ^^ { case left ~ _ ~ right => Lshift(left, right)}
    | simpleExpr ~ "RSHIFT" ~ simpleExpr ^^ { case left ~ _ ~ right => Rshift(left, right)}
    | simpleExpr
    )

    def instruction: Parser[(String, Expr)] = (
      expr ~ "->" ~ ident ^^ { case expr ~ _ ~ ident => ident -> expr}
    )

    parseAll(instruction, s) match {
      case Success(result, next) => result
      case NoSuccess(msg, next) => throw new RuntimeException(s"Assignment parsing error: $msg ($next)")
    }
  }

  def parseInstructions(input: String): Map[String, Expr] = input.linesIterator.map(parseInstruction).toMap


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.evalA(input))
    println(Part2.evalA(input))
  }
}
