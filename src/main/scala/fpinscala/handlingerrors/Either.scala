package fpinscala.handlingerrors

//  Done Exercise 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the
//    Right value.
//  Done Exercise 4.7 Implement sequence and traverse for Either. These should return the first error
//    that’s encountered, if there is one.
//  TODO Exercise 4.8 In this implementation, map2 is only able to report one error, even if both the
//    name and the age are invalid. What would you need to change in order to report both errors?
//    Would you change map2 or the signature of mkPerson? Or could you create a new data type that
//    captures this requirement better than Either does, with some additional structure? How would
//    orElse, traverse, and sequence behave differently for that data type?

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[AA >: A](default: => AA): AA
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}
case class Left[E](value: E) extends Either[E, Nothing] {

  override def map[B](f: Nothing => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this

  override def orElse[AA >: Nothing](default: => AA): AA = default

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

  case class Right[A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)

    override def orElse[AA >: A](default: => AA): AA = value

    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap(a => b.map(b => f(a, b)))
  }

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldLeft(identity: Either[E, List[A]] => Either[E, List[A]]) {
      (a, e) => z => a(e.flatMap(e => z.map(z => e::z)))
    }(Right(List()): Either[E, List[A]])

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft(identity: Either[E, List[B]] => Either[E, List[B]]) {
      (acc, element) => z => acc(f(element).flatMap(h => z.map(t => h::t)))
    }(Right(Nil): Either[E, List[B]])
}
