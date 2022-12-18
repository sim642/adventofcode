package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos3

case class Box3(min: Pos3, max: Pos3) extends BoxOps[Pos3, Box3] {
  override def factory: BoxFactory[Pos3, Box3] = Box3

  override def iterator: Iterator[Pos3] = {
    for {
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
    } yield Pos3(x, y, z)
  }

  /*def closestTo(pos: Pos3): Pos3 = {
    Pos3(
      clamp(min.x, max.x)(pos.x),
      clamp(min.y, max.y)(pos.y),
      clamp(min.z, max.z)(pos.z),
    )
  }*/

  def diffSplit(that: Box3): Seq[Box3] = {
    this intersect that match {
      case None => Seq(this)
      case Some(inter) =>

        def make(a: Pos3, b: Pos3): Option[Box3] = {
          if (a <= b)
            Some(Box3(a, b))
          else
            None
        }

        Seq(
          (Pos3(min.x, min.y, min.z), Pos3(inter.min.x - 1, max.y, max.z)),
          (Pos3(inter.max.x + 1, min.y, min.z), Pos3(max.x, max.y, max.z)),
          (Pos3(inter.min.x, min.y, min.z), Pos3(inter.max.x, inter.min.y - 1, max.z)),
          (Pos3(inter.min.x, inter.max.y + 1, min.z), Pos3(inter.max.x, max.y, max.z)),

          (Pos3(inter.min.x, inter.min.y, min.z), Pos3(inter.max.x, inter.max.y, inter.min.z - 1)),
          (Pos3(inter.min.x, inter.min.y, inter.max.z + 1), Pos3(inter.max.x, inter.max.y, max.z)),
        ).flatMap(make)
    }
  }
}

object Box3 extends BoxFactory[Pos3, Box3] {

}
