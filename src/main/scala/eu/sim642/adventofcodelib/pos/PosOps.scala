package eu.sim642.adventofcodelib.pos

trait PosOps[A] {
  def +(that: A): A

  def manhattanDistance(that: A): Int
}
