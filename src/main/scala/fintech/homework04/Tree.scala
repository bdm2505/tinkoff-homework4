package fintech.homework04


sealed trait Tree[+A] {


  def size: Int = fold(_ => 1)(_ + _)

  def fold[T](funLeaf: A => T)(funBranch: (T, T) => T): T = {
    def foldRec(tree: Tree[A]): T = tree match {
      case leaf: Leaf[A] =>
        funLeaf(leaf.value)
      case branch: Branch[A] =>
        funBranch(foldRec(branch.left), foldRec(branch.right))
    }

    foldRec(this)
  }

  def max[T >: A](implicit ord: Ordering[T]): T = fold(identity[T])(ord.max)

  def depth: Int = fold(_ => 1)(_ max _ + 1)

  def map[B](fun: A => B): Tree[B] = fold[Tree[B]](elem => Leaf(fun(elem)))(Branch(_, _))

}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def apply[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  implicit def toBranch[A](t: (Tree[A], Tree[A])): Branch[A] = Branch(t._1, t._2)

  implicit class LeafRich[T](val value: T) extends AnyVal {
    def l: Tree[T] = Leaf(value)
  }

}
