package fpinscala.handlingerrors

import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {
  "map" should "map value inside some or just return None if option is none" in {
    Some(1).map(_ * 2) shouldBe Some(2)
    None.map(identity) shouldBe None
  }

  "flatMap" should "map value inside option to option" in {
    Some(1) flatMap (((a: Int) => a * 2) andThen (Some(_))) shouldBe Some(2)
    Some(1) flatMap (_ => None) shouldBe None
    None flatMap ((2 * (_: Int)) andThen (Some(_))) shouldBe None
  }

  "getOrElse" should "get value inside some or return default value" in {
    Some(33).getOrElse(1) shouldBe 33
    None.getOrElse("default") shouldBe "default"
  }

  "orElse" should "return self if is Some or return option passed in" in {
    Some(1) orElse Some(2) shouldBe Some(1)
    None orElse Some(2) shouldBe Some(2)
  }

  "filter" should "return self if pass filter else return none" in {
    Some(3) filter (_ > 0) shouldBe Some(3)
    Some(3) filter (_ < 2) shouldBe None
  }

  it should "return none if self is none" in {
    (None: Option[String]) filter (_.isEmpty) shouldBe None
  }

  "variance" should "compute variance" in {
    variance(Seq(1, 2, 3, 4)) shouldBe Some(1.25)
    variance(Seq()) shouldBe None
  }

  "map2" should "map two value" in {
    Option.map2(Some(1), Some(2))(_ + _) shouldBe Some(3)

    Option.map2(None, Some(2))((_, _)) shouldBe None

    Option.map2(Some(2), None)((_, _)) shouldBe None
  }

  "sequence" should "combine list of option into option of list" in {
    Option.sequence(Seq(Some(1), Some(2))) shouldBe Some(Seq(1, 2))
  }

  it should "result none if list contain none" in {
    Option.sequence(List(Some("test"), None, Some("suffix"))) shouldBe None
  }

  "traverse" should "convert list to list of option and sequence on it" in {
    Option.traverse(List(1, 2, 3, 4))(a => Some(a.toString)) shouldBe (Some(List("1", "2", "3", "4")))
  }
}
