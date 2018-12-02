package variance.hw

trait BiTree[+T] {
  val value: T
  val left: BiTree[T]
  val right: BiTree[T]
  def add[U >: T](value: U)(implicit ord: ContrVOrdering[U]): BiTree[U]
  def contains[U >: T](v: U)(implicit ord: ContrVOrdering[U]): Boolean
}

object Empty extends BiTree[Nothing] {
  lazy val value: Nothing = throw new IllegalAccessException
  lazy val left: BiTree[Nothing] = throw new IllegalAccessException
  lazy val right: BiTree[Nothing] = throw new IllegalAccessException

  def add[U >: Nothing](value: U)(implicit ord: ContrVOrdering[U]): BiTree[U] =
    Node(value, Empty, Empty)
  override def toString: String = "*"
  def contains[U >: Nothing](v: U)(implicit ord: ContrVOrdering[U]): Boolean = false
}

case class Node[+T](v: T, l: BiTree[T], r: BiTree[T]) extends BiTree[T] {
  val value: T = v
  val left: BiTree[T] = l
  val right: BiTree[T] = r

  override def add[U >: T](v: U)(implicit ord: ContrVOrdering[U]): BiTree[U] = {
    if (ord.compare(v, value) > 0) {
      Node(value, left, right.add(v))
    } else if ((ord.compare(v, value) < 0)) {
      Node(value, left.add(v), right)
    } else Node(value, left, right)
  }

  override def contains[U >: T](v: U)(implicit ord: ContrVOrdering[U]): Boolean = {
    if (ord.compare(v, value) == 0) return true
    else if (ord.compare(v, value) > 0) right.contains(v)
    else left.contains(v)
  }
  override def toString: String = s"{ ${left.toString}  ${value}  ${right.toString} }"
}
