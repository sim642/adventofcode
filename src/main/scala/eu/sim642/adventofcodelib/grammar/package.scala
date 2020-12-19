package eu.sim642.adventofcodelib

package object grammar {
  // TODO: replace Either with custom type
  type ProductionBody[N, T] = Seq[Either[N, T]]
  type Production[N, T] = (N, ProductionBody[N, T])

  // TODO: use MultiDict
  type Grammar[N, T] = Map[N, Set[ProductionBody[N, T]]]
}
