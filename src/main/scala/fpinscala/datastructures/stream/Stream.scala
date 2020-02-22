package fpinscala.datastructures.stream

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  def take(n: Int): Stream[A] = if (n <= 0) Empty else this match {
    case Empty => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = if (n <= 0) this else this match {
    case Empty => Empty
    case Cons(_, t) => t().drop(n - 1)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => if (f(h())) Cons(h, () => t().takeWhile(f)) else Empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) =>f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A]) { (a, b) =>
      if (f(a)) Cons(() => a, () => b) else Empty
    }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A]) { (a, _) =>
      Some(a)
    }

  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A]){ (a, b) =>
    if (f(a)) Cons(() => a, () => b) else b
  }

  def append[AA >: A](as: Stream[AA]): Stream[AA] = foldRight(as)((a, b) => Cons(() => a, () => b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

