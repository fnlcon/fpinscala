package fpinscala

trait Parsers[ParseError, Parser[+_]] {
  self =>

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
     f(a)

  def orString(s1: String, s2: String): Parser[String] =
    or(string(s1), string(s2))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

//  def count: Parser[Int]
//
//  def oneMore: Parser[Int]
//
//  def aZeroBMore: Parser[(Int, Int)]
//
//  def `match`(c: Char): Parser[Int]


  //<editor-fold desc="a">
  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  val numA: Parser[Int] = char('a').many.map(_.size)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  //</editor-fold>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  //<editor-fold desc="9.1">
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    (p ** p2).map { case (a, b) => f(a, b) }

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many) {case (a, b) => a::b}
  //</editor-fold>

  //<editor-fold desc="9.2">
    // product a b = map2 a b \a b = (a, b)
  //</editor-fold>

  // 9.3 Hard: Before continuing, see if you can define many in terms of or, map2, and succeed.
  def many_[A](p: Parser[A]): Parser[List[A]] = map2(p, many_(p))(_ :: _) | succeed(Nil)

  // 9.4 Using map2 and succeed, implement the listOfN combinator from earlier.
  def listOfN_[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 1) map2(p, succeed(Nil: List[A]))(_::_)
    else map2(p, listOfN_(n - 1, p))(_::_)
}
