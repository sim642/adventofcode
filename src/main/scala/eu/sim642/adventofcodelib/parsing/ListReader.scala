package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.input.{NoPosition, Position, Reader}

case class ListReader[A](list: List[A], index: Int = 0) extends Reader[A] {
  override def first: A = list.head

  override def rest: Reader[A] = {
    if (atEnd)
      this // required by rest scaladoc
    else
      ListReader(list.tail, index + 1)
  }

  override def pos: Position = ListPosition(index)

  override def atEnd: Boolean = list.isEmpty
}
