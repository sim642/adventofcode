package eu.sim642

package object adventofcodelib {
  type Grid[A] = Vector[Vector[A]]

  type LazyMap[K, +V] = Map[K, LazyCell[V]]
}
