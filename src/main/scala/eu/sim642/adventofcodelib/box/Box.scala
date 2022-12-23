package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos

import scala.math.Numeric.Implicits.infixNumericOps

case class Box(min: Pos, max: Pos) extends BoxOps[Pos, Box] {
  override def factory: BoxFactory[Pos, Box] = Box

  override def iterator: Iterator[Pos] = {
    for {
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
    } yield Pos(x, y)
  }

  override def size[C](using cNumeric: Numeric[C]): C = {
    val d = max - min + Pos(1, 1)
    cNumeric.fromInt(d.x) * cNumeric.fromInt(d.y)
  }

  def diffSplit(that: Box): Seq[Box] = {
    this intersect that match {
      case None => Seq(this)
      case Some(inter) =>

        def make(a: Pos, b: Pos): Option[Box] = {
          if (a <= b)
            Some(Box(a, b))
          else
            None
        }

        Seq(
          (Pos(min.x, min.y), Pos(inter.min.x - 1, max.y)),
          (Pos(inter.max.x + 1, min.y), Pos(max.x, max.y)),
          (Pos(inter.min.x, min.y), Pos(inter.max.x, inter.min.y - 1)),
          (Pos(inter.min.x, inter.max.y + 1), Pos(inter.max.x, max.y)),
        ).flatMap(make)
    }
  }
}

object Box extends BoxFactory[Pos, Box] {

}
