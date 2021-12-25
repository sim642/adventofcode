package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

trait ExtraParsers extends Parsers {

  def any: Parser[Elem] = (in: Input) => {
    if (in.atEnd)
      Failure("any", in)
    else
      Success(in.first, in.rest)
  }

  private case class LengthedInput(in: Input, left: Int) extends Input {
    override def first: Elem = in.first

    override def rest: Reader[Elem] = {
      if (atEnd)
        this // required by rest scaladoc
      else
        LengthedInput(in.rest, left - 1)
    }

    override def pos: Position = in.pos

    override def atEnd: Boolean = left <= 0 || in.atEnd
  }

  def lengthed[A](length: Int, p: => Parser[A]): Parser[A] = (in: Input) => {
    p(LengthedInput(in, length)) match {
      case Success(result, next) => Success(result, in.drop(length))
      case Failure(msg, next) => Failure(msg, in)
      case Error(msg, next) => Error(msg, in)
    }
  }
}
