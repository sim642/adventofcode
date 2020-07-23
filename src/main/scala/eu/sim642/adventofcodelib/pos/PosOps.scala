package eu.sim642.adventofcodelib.pos

trait PosOps[A <: PosOps[A]] {
  def +(that: A): A
  def *:(k: Int): A

  def unary_- : A = -1 *: this
  def -(that: A): A = this + (-that)

  def manhattanDistance(that: A): Int
}
