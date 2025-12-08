package eu.sim642.adventofcodelib.graph

import eu.sim642.adventofcodelib.UnionFind

object Kruskal {

  def iterate[A](nodes: Seq[A], sortedEdges: IterableOnce[(A, A)]): Iterator[(UnionFind[A], Option[(A, A)])] = {
    sortedEdges.iterator
      .scanLeft((new UnionFind(nodes), Option.empty[(A, A)]))({ case ((uf, _), edge@(p1, p2)) =>
        if (uf.sameRepr(p1, p2))
          (uf, None)
        else
          (uf.unioned(p1, p2), Some(edge))
      })
  }

  def iterateEdges[A](nodes: Seq[A], sortedEdges: IterableOnce[(A, A)]): Iterator[(A, A)] =
    iterate(nodes, sortedEdges).flatMap(_._2).take(nodes.size - 1)
}
