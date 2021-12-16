package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.input.{NoPosition, Position, Reader}

case class ListReader[A](list: List[A]) extends Reader[A] {
  override def first: A = list.head

  override def rest: Reader[A] = ListReader(list.tail)

  override def pos: Position = NoPosition // TODO: something better

  override def atEnd: Boolean = list.isEmpty
}
