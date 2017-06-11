package HuffmanScala

sealed trait BinaryTree[A] {
  val data: A
}

case class NonLeafNode[A](data: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

case class LeafNode[A](data: A) extends BinaryTree[A]
