package HuffmanScala

sealed trait BinaryTree[A] {
  val data: A
}

case class NonLeafNode[A](val data: A, val left: BinaryTree[A], val right: BinaryTree[A]) extends BinaryTree[A]

case class LeafNode[A](val data: A) extends BinaryTree[A]
