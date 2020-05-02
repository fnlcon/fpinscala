package fpinscala.datastructures.stream

sealed trait Stream[+A] {
  import Stream._

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

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, t) => Some(f(h()), t())
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold(n, this) {
    case (n, _) if n <= 0 => None
    case (n, s) => s match {
      case Empty => None
      case Cons(h, t) => Some(h(), (n - 1, t()))
    }
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Empty => None
    case Cons(h, _) if !f(h()) => None
    case Cons(h, t) => Some(h(), t())
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold(this, bs) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold(this, s2) {
    case (Empty, Empty) => None
    case (Empty, Cons(h, t)) =>Some((None, Some(h())), (Empty, t()))
    case (Cons(h, t), Empty) =>Some((Some(h()), None), (t(), Empty))
    case (Cons(h1, t1), Cons(h2, t2)) =>Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  def startsWith[A](s: Stream[A]): Boolean = s match {
    case Empty => true
    case Cons(h1, t1) => this match {
      case Empty => false
      case Cons(h2, t2) =>
        if (h1() != h2()) false
        else t2().startsWith(t1())
    }
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case s@Cons(_, t) => Some(s, t())
    case Empty => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = ???
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibonacci(a: Int, b: Int): Stream[Int] =
      cons(a, fibonacci(b, a + b))

    fibonacci(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) =>cons(a, unfold(s)(f))
      case None => empty
    }

  object ViaUnfold {
    def fibs: Stream[Int] = unfold(0, 1) {
      case (a, b) => Some(a, (b, a + b))
    }

    def from(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

    def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

    def ones: Stream[Int] = constant(1)
  }
}

