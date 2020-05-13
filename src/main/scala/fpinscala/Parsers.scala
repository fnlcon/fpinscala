package fpinscala

import fpinscala.JSON.{JArray, JNumber, JObject, JString}

import scala.util.matching.Regex

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

  //</editor-fold>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

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


  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // 9.6
  def nchar(c: Char): Parser[String] =
    regex("\\d".r).flatMap(d => regex(s"${c}{${d}}".r).map(e => d ++ e))

  // 9.7
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  def mapViaFlatMap[A, B](p: Parser[A])(f: A => B) =
    p.flatMap(a => succeed(f(a)))

  def repeat[A, B](element: Parser[A], separator: Parser[B]): Parser[List[A]] =
    ((element ** separator map (_._1)).many ** element map {
      case (es, e) => es ++ List(e)
    }) | (element map (List(_))) | succeed(List())

  def first[A](p: Parser[(A, _)]): Parser[A] = p map (_._1)

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    // strings(s)
    // regex(s)
    // slice(s)
    // succed(a)
    // flatMap(p)
    // or(p1, p2)

    val quote: Parser[String] = "\""
    val open_obj: Parser[String] = "{"
    val close_obj: Parser[String] = "}"

    val open_arr:Parser[String] = "["
    val close_arr:Parser[String] = "]"

    val jString = quote ** regex("[0-9a-zA-Z~!@#$%^&*()_+`\\-={}\\[\\]|\\\\s<>?,./]".r) ** quote map {case ((_, str), _) => JString(str)}
    val jNumber: Parser[JSON] = "[1-9]\\d*(\\.?\\d+)?".r.map(n => JNumber(n.toDouble))
    val comma: Parser[String] = ","

    def jArrItems(p: Parser[JObject]): Parser[JSON] = jNumber | jString | p

    def jArray(p: Parser[JObject]): Parser[JSON] = open_arr ** repeat(jArrItems(p), comma) ** close_arr map {
      case ((_, is), _) => JArray(is.toIndexedSeq)
    }


    def colon: Parser[String] = ":"

    def whitespace: Parser[String] = "[\\s\\r\\n]*".r

    def jObject: Parser[JObject] = open_obj ** jObjectContent ** close_obj map {
      case ((_, es), _) => JObject(es.toMap)
    }

    def jObjectContent =
      repeat(
        jString ** comma ** (jString | jNumber | jArray(jObject) | jObject) map {
          case ((key, _), value) => (key.get, value)
        },
        comma)

    jNumber | jString | jObject | jArray(jObject)
  }


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }


}


trait JSON
object JSON {
  case object JNull extends JSON
  case class  JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
