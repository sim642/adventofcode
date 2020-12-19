package eu.sim642.adventofcodelib

package object grammar {
  // TODO: replace Either with custom type
  type Production[N, T] = (N, Seq[Either[N, T]])

  type Grammar[N, T] = Seq[Production[N, T]]
}
