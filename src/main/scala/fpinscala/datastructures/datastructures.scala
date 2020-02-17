package fpinscala

import scala.annotation.tailrec

package object datastructures {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](a: A*): List[A] = {
      if (a.isEmpty) Nil
      else Cons(a.head, apply(a.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def product(ints: List[Double]): Double = ints match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def foldRight[A, B](items: List[A], z: B)(f: (A, B) => B): B = items match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ints: List[Int]): Int =
      foldRight(ints, 0)(_ + _)

    def product2(ns: List[Double]): Double =
      foldRight(ns, 1.0)(_ * _)

    // 3.9
    def length[A](xs: List[A]): Int =
      foldRight(xs, 0)((_, size) => size + 1)

    // 3.10
    @tailrec
    def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // 3.11
    object P3_11 {
      def sum(ints: List[Int]): Int =
        foldLeft(ints, 0)(_ + _)

      def product(ns: List[Double]): Double =
        foldLeft(ns, 1.0)(_ * _)
    }

    // 3.12
    def reverse[A](xs: List[A]): List[A] =
      foldLeft(xs, Nil: List[A]) { (tail, head) =>
        Cons(head, tail)
      }

    // 3.12
    def reverseFoldRight[A](xs: List[A]) = foldRight(xs, identity: List[A] => List[A]) {
      (a, b) => (bb: List[A]) => b(Cons(a, bb))
    }(Nil)

    // 3.13 FoldLeft by FoldRight
    // FoldRight by FoldLeft
    object P3_13 {
      def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
        datastructures.List.foldRight(xs, identity: B => B) {
          (a, b) => z => b(f(z, a))
        }(z)

      def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        datastructures.List.foldLeft(as, identity: B => B) {
          (b, a) => z => b(f(a, z))
        }(z)
    }

    def append[A](xs: List[A], x: A): List[A] =
      foldRight(xs, Cons(x, Nil))(Cons(_, _))

    def appendFoldLeft[A](xs: List[A], x: A): List[A] =
      foldLeft(xs, identity: List[A] => List[A]) {
        (b, a) => (bb: List[A]) => b(Cons(a, bb))
      }(List(x))

    // 3.15
    def flat[A](xss: List[List[A]]) =
      foldRight(xss, Nil: List[A]) {
        (a, b) => foldRight(a, b) { (a, b) => Cons(a, b) }
      }

    // 3.16
    def mapAddOne(is: List[Int]): List[Int] =
      foldRight(is, Nil: List[Int])((a, b) => Cons(a + 1, b))

    // 3.17
    def mapDoubleToString(ds: List[Double]): List[String] =
      foldRight(ds, Nil: List[String])((a, b) => Cons(a.toString, b))

    // 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A]) { (a, b) =>
        if (f(a)) Cons(a, b) else b
      }

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, Nil: List[B]) { (a, b) =>
        foldRight(f(a), b)((a, b) => Cons(a, b))
      }

    // 3.21
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
      flatMap(as) { (a) =>
        if (f(a)) List(a) else Nil
      }

    // 3.22
    def addTwoList(as: List[Int], bs: List[Int]): List[Int] =
      as match {
        case Nil => Nil
        case Cons(a, as) => bs match {
          case Cons(b, bs) => Cons(a + b, addTwoList(as, bs))
          case Nil => Nil
        }
      }

    // 3.23
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
      as match {
        case Nil => Nil
        case Cons(a, as) => bs match {
          case Cons(b, bs) => Cons(f(a, b), zipWith(as, bs)(f))
          case Nil => Nil
        }
      }

    // 3.24
    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def prefix(sup: List[A], sub: List[A]): Boolean = sub match {
        case Nil => true
        case Cons(s, ss) => sup match {
          case Cons(p, ps) => if (s == p) prefix(ps, ss) else false
          case _ => false
        }
      }

      sup match {
        case Cons(t, ts) => if (prefix(sup, sub)) true else hasSubsequence(ts, sub)
        case Nil => prefix(sup, sub)
      }
    }
  }



  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(max) => max
      case Branch(left, right) => maximum(left) max maximum(right)
    }

    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(left, right) => (depth(left) max depth(right)) + 1
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }

    def fold[A, B](tree: Tree[A])(f: A => B)(combiner: (B, B) => B): B =
      tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => combiner(fold(left)(f)(combiner), fold(right)(f)(combiner))
      }

    object Fold {
      def size[A](tree: Tree[A]): Int =
        Tree.fold(tree)(t => 1)((a, b) => a + b + 1)

      def maxinum(tree: Tree[Int]): Int =
        Tree.fold(tree)(identity)(_ max _)

      def depth[A](tree: Tree[A]): Int =
        Tree.fold(tree)(_ => 0)((l, r) => (l max r) + 1)

      def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        Tree.fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
    }
  }
}