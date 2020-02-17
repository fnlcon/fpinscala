package fpinscala.datastructures
import List._

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import scala.collection.mutable.Stack

class DataStructureSpec extends FlatSpec with Matchers {
  "List" should "not empty" in {
    assert(List(1, 2, 3, 4) !== null)
  }

  it should "has apply" in {
    assert((List.apply _) != null)
  }

  "foldRight" should "fold from right" in {
    List.foldRight(List(1, 2, 3, 4, 5), 0)(_ + _) should equal(15)
  }

  "sum2" should "use foldRight" in {
    sum2(List(1, 2, 3, 4, 5)) should equal(15)
  }

  "product2" should "use foldRight" in {
    product2(List(1.0, 2.0, 5.0)) shouldBe (10.0)
  }

  // TODO review practice 3.7

  "foldRight with Nil and Cons" should "construct a equal List" in {
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should equal(List(1, 2, 3))
  }

  "fpinscala.datastructures.length" should "return length of list" in {
    List.length(List(1, 2, 3, 4, 5, 6, 7, 8)) shouldBe (8)
  }

  "foldLeft" should "fold from left" in {
    val stack = Stack[Int]()
    foldLeft(List(1, 2, 3, 4, 5), 0) { (b, a) =>
      stack.push(a)
      b
    }

    stack.pop() shouldBe (5)
  }

  "foldLeft sum" should "sum int list" in {
    P3_11.sum(List(1, 2, 3, 4, 5)) shouldBe 15
  }

  "foldLeft product" should "compute product of list" in {
    P3_11.product(List(1.0, 2.0, 3.0, 4.0, 5.0)) shouldBe 120
  }

  "reverse" should "reverse list" in {
    reverse(List(1, 2, 3)) should equal(List(3, 2, 1))
  }

  "reverse right" should "reverse list" in {
    reverseFoldRight(List(1, 2, 3)) should equal(List(3, 2, 1))
  }

  "3.13 foldLeft by foldRight" should "reverse list" in {
    P3_13.foldLeft(List(1, 2, 3), Nil: List[Int])((z, i) => Cons(i, z)) should equal(List(3, 2, 1))
  }

  "3.13 foldRight by foldLeft" should "return same list" in {
    P3_13.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should equal(List(1, 2, 3))
  }

  "3.14 append in foldRight" should "append element after list" in {
    append(List(1, 2, 3, 4), 5) should equal(List(1, 2, 3, 4, 5))
  }

  "3.14 append in foldLeft" should "append element after list" in {

    appendFoldLeft(Nil: List[Int], 1) should equal(List(1))

    appendFoldLeft(List(1, 2, 3, 4), 5) should equal(List(1, 2, 3, 4, 5))
  }
  "3.15 flat " should "concat list in list to a single list" in {
    flat(List(List(1), List(2), List(3))) should equal(List(1, 2, 3))
  }

  "3.16 map add one" should "add 1 to every element in list and return a new list" in {
    mapAddOne(List(1, 2, 3)) should equal(List(2, 3, 4))
  }

  "3.17 mapDoubleToString" should "return a new List with double convert to string" in {
    mapDoubleToString(List(0.0D, 1.0D, 2.0D)) should equal(List("0.0", "1.0", "2.0"))
  }

  "3.18 map" should "map element in list" in {
    map(List(1, 2, 3))(_ + 1) should equal(List(2, 3, 4))
  }

  "3.19 filter" should "return satisfied element" in {
    filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) should equal(List(2, 4))
  }

  "3.20 flatMap" should "map element to list and concat all list" in {
    flatMap(List(1, 2))(a => List(a, a)) should equal(List(1, 1, 2, 2))
  }

  "3.21 filterViaFlatMap" should "filter element by predicate" in {
    filterViaFlatMap(List(1, 2, 3))(_ % 2 == 1) should equal(List(1, 3))
  }

  "3.22 addTwoList" should "constructs a new list by adding corresponding elements" in {
    addTwoList(List(1, 2, 3), List(1, 2, 3)) should equal(List(2, 4, 6))
  }

  "3.23 zipWith" should "combine two list with function" in {
    zipWith(List(1, 2, 3), List(3, 2, 1))(_ + _) should equal(List(4, 4, 4))
  }

  "3.24 hasSubsequence" should "return true if has sub sequence" in {
    hasSubsequence(List(1, 2, 3), List(2)) should be(true)
    hasSubsequence(Nil, Nil) shouldBe true
    hasSubsequence(List(2, 1), List(3)) shouldBe false
    hasSubsequence(List(3, 2, 1, 6), List(1, 6)) shouldBe true
  }

  "3.25 size" should "count the number of nodes" in {
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) shouldBe 7
  }

  "3.26 max" should "return max number in tree" in {
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) shouldBe 4
  }

  "3.27 depth" should "return depth of tree" in {
    Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) shouldBe 2
  }

  "3.28 map" should "apply function to each value" in {
    Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(a => List(a)) should equal(Branch(Branch(Leaf(List(1)), Leaf(List(2))), Leaf(List(3))))
  }

  "3.29 fold" should "generalize size, maximum, depth" in {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    // size
    Tree.Fold.size(tree) shouldBe 5

    // maxinum
    Tree.Fold.maxinum(tree) shouldBe 3

    // depth
    Tree.Fold.depth(tree) shouldBe 2
  }
}
