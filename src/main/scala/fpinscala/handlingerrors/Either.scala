package fpinscala.handlingerrors

sealed trait Either[+E, +A]
case class Left[E]() extends Either[E, Nothing]
case class Right[A]() extends Either[Nothing, A]

object Either {

}
