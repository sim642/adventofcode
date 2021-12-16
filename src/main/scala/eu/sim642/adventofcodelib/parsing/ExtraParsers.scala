package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.combinator.Parsers

trait ExtraParsers extends Parsers {

  def any: Parser[Elem] = (in: Input) => {
    if (in.atEnd)
      Failure("any", in)
    else
      Success(in.first, in.rest)
  }
}
