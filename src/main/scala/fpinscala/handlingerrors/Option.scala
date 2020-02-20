package fpinscala.handlingerrors

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)
}

object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default
}

case class Some[A](get: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def getOrElse[B >: A](default: => B): B = get
}


object Option {
  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa.flatMap(a => ob.map(b => f(a, b)))


  def sequence[A](a: Seq[Option[A]]): Option[Seq[A]] =
    a.foldRight(Some(Seq()): Option[Seq[A]]) {
      (head, tail) => {
        tail.flatMap(t => head.map(h => h +: t))
      }
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldLeft(identity: Option[List[B]] => Option[List[B]]) {
      (accumulator, element) => z => accumulator(f(element).flatMap(h => z.map(t => h::t)))
    }(Some(Nil): Option[List[B]])
}