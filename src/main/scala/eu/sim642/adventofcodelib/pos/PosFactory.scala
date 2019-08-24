package eu.sim642.adventofcodelib.pos

trait PosFactory[A <: PosOps[A]] {
  val zero: A
}
