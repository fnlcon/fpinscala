package fpinscala.handlingerrors

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {
  "Right" should "map it's value by map" in {
    Right(1) map (_ * 2) shouldBe Right(2)
  }

  it should "flatMap it value to returned Either" in {
    Right(1) flatMap (a => Right(2 * a)) shouldBe Right(2)
  }

  it should "return it's value by orElse" in {
    Right(1) orElse 2 shouldBe 1
  }

  "Left" should "map to Left" in {
    (Left(1): Either[Int, Int]) map (_ * 2) shouldBe Left(1)
  }

  it should "flatMap to Left" in {
    (Left(1): Either[Int, Int]) map (a => Right(2 * a)) shouldBe Left(1)
  }

  it should "should return passed in by orElse" in {
    (Left(1): Either[Int, Int]) orElse 2 shouldBe 2
  }

  "Either's map2" should "map two either's right value" in {
    Right(1).map2(Right("right")){ (a, b) => (a, b) } shouldBe Right(1, "right")
  }

  it should "return first Left if any" in {
    Right(1).map2(Left("left"): Either[String, Int]){ (a, b) => (a, b) } shouldBe Left("left")
    (Left(2): Either[Int, String]).map2(Right("right")){ (a, b) => (a, b) } shouldBe Left(2)
  }

  "sequence" should "sequence list of either to either of list" in {
    Either.sequence(List(Right(1), Right(2), Right(3), Right(4))) shouldBe Right(List(1, 2, 3, 4))
  }

  it should "sequence to first Left" in {
    Either.sequence(List(Right(1), Left(2), Left(3), Right(4))) shouldBe Left(2)
  }

  "traverse" should "map and sequence list of either" in {
    val intToRightString: Int => Either[String, Int] =
      a => Right(a + 1)

    Either.traverse(List(1, 2, 3, 4))(intToRightString) shouldBe Right(List(2, 3, 4, 5))
  }

  it should "return first Left map" in {
    Either.traverse(List(0, 0, 1, 2, 3))(n => if (n == 0) Right(0) else Left(n)) shouldBe Left(1)
  }
}
