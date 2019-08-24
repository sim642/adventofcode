package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.BoxPosOps

trait BoxFactory[A <: BoxPosOps[A], B <: BoxOps[A, B]] {
  def apply(min: A, max: A): B

  def apply(pos: A): B = apply(pos, pos)

  def bounding(poss: IterableOnce[A]): B = {
    poss.iterator.map(apply).reduce(_ union _)
  }
}
