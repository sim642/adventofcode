package eu.sim642.adventofcodelib.pos

trait BoxPosOps[A <: BoxPosOps[A]] extends PosOps[A] {
  def <=(that: A): Boolean
  infix def min(that: A): A
  infix def max(that: A): A
}
