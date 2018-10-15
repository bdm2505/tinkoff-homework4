package fintech.homework04

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  import Tree._

  val treeInt = Tree(1.l -> Tree(43.l, 89 l), Tree(4.l -> 4.l, 56.l -> 920.l))
  val treeString = Tree("?".l -> Tree("ok".l -> "fail".l, "yes".l), "hello".l)

  it should "tests functions map, max, depth and size" in {
    treeInt.max shouldEqual 920
    treeString.max shouldEqual Seq("?", "ok", "fail", "yes", "hello").max
    54.l.max shouldEqual 54

    treeInt.depth shouldEqual 4
    treeString.depth shouldEqual 3
    54.l.depth shouldEqual 1

    treeInt.size shouldEqual 7
    treeString.size shouldEqual 5
    54.l.size shouldEqual 1

    2.l -> 4.l map (_ * 2) shouldEqual Tree(4.l, 8.l)
    "ok".l -> Tree("no" l, "io" l) map (_ * 2) shouldEqual Tree("okok" l, Tree("nono" l, "ioio" l))
  }


}
