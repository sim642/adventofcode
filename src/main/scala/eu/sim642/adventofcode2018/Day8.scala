package eu.sim642.adventofcode2018

object Day8 {

  case class Tree(childen: Seq[Tree], metadata: Seq[Int])

  def metadataSum(tree: Tree): Int = tree.childen.map(metadataSum).sum + tree.metadata.sum

  def value(tree: Tree): Int = {
    if (tree.childen.isEmpty)
      tree.metadata.sum
    else {
      tree.metadata
        .filter(i => 1 <= i && i <= tree.childen.length)
        .map(i => tree.childen(i - 1))
        .map(value)
        .sum
    }
  }

  type Parser[A, B] = A => (B, A)

  def replicateParser[A, B](n: Int, f: Parser[A, B]): Parser[A, Seq[B]] = { a =>
    if (n <= 0)
      (Seq.empty, a)
    else {
      val (b, a2) = f(a)
      val (bs, a3) = replicateParser(n - 1, f)(a2)
      (b +: bs, a3)
    }
  }

  def parseTree(seq: List[Int]): Tree = {
    def treeParser: Parser[List[Int], Tree] = {
      case childrenCount :: metadataCount :: tl =>
        val (children, seq) = replicateParser(childrenCount, treeParser)(tl)
        val (metadata, seq2) = seq.splitAt(metadataCount)
        (Tree(children, metadata), seq2)
    }

    treeParser(seq)._1
  }

  def parseTree(input: String): Tree = parseTree(input.split(" ").map(_.toInt).toList)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(metadataSum(parseTree(input)))
    println(value(parseTree(input)))
  }
}
