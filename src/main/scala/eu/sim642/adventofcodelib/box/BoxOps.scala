package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.BoxPosOps

trait BoxOps[A <: BoxPosOps[A], B <: BoxOps[A, B]] {
  val min: A
  val max: A

  require(min <= max)

  def factory: BoxFactory[A, B]

  def contains(pos: A): Boolean =
    min <= pos && pos <= max

  def intersect(that: B): Option[B] = {
    val intersectMin = min max that.min
    val intersectMax = max min that.max
    if (intersectMin <= intersectMax)
      Some(factory(intersectMin, intersectMax))
    else
      None
  }
}
