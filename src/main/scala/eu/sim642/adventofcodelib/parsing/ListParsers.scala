package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.combinator.Parsers

trait ListParsers[A] extends Parsers {

  override type Elem = A


  def parse[T](p: Parser[T], in: List[A]): ParseResult[T] =
    p(ListReader(in))

  def parseAll[T](p: Parser[T], in: List[A]): ParseResult[T] =
    parse(phrase(p), in)
}
