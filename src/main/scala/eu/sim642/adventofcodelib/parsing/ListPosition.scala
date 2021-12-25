package eu.sim642.adventofcodelib.parsing

import scala.util.parsing.input.Position

case class ListPosition(index: Int) extends Position {
  override def line: Int = 0
  override def column: Int = index

  override protected def lineContents: String = ""
  override def toString: String = index.toString
  override def longString: String = toString
}
