package fpinscala.datastructures.stream

import fpinscala.datastructures.stream.Stream.cons
import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {
  "5.1 toList" should "convert stream to list" in {
    Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  "5.2 take(n)" should "take first n element" in {
    Stream(1, 2, 3, 4, 5).take(3).toList shouldBe List(1, 2, 3)
  }

  "5.2 drop(n)" should "drop first n element" in {
    Stream(1, 2, 3, 4, 5).drop(3).toList shouldBe List(4, 5)
  }

  "5.3 takeWhile" should "returning all starting elements of a Stream that match the given predicate." in {
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

  "5.4 forAll" should "return when first element not math predicate" in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, Stream.empty)))
    stream.forAll(_ < 2) shouldBe false
    count shouldBe 2
  }

  "5.5 takeWhileViaFoldRight" should "returning all starting elements of a Stream that match the given predicate." in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))

    stream.takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)

    count shouldBe 3
  }

  "5.6 headOption using foldRight" should "return first element in stream in option" in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))
    stream.headOptionViaFoldRight shouldBe Some(1)
    count shouldBe 1
  }

  "5.7 map" should "map element in stream lazily" in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))
    val s = stream.map(identity)

    count shouldBe(1)
    s.toList shouldBe List(1, 2, 3, 4, 5)
  }

  "5.7 filter" should "filter element out stream lazily" in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))

    val s = stream.filter(a => a % 2 == 1)
    count shouldBe(1)
    s.toList shouldBe List(1, 3, 5)

    count shouldBe 5
  }

  "5.7 append" should "append two stream" in {
    import Stream.cons
    var count = 0
    val s1 = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))
    val s2 = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))

    val s3 = s1.append(s2)

    count shouldBe 1

    s3.toList shouldBe List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
  }

  "5.8 constant" should "return constant stream" in {
    Stream.constant(1).take(10).toList shouldBe List.fill(10)(1)
  }

  "5.9 from" should "construct sequence stream from n" in {
    Stream.from(10).take(3).toList shouldBe List(10, 11, 12)
  }

  "5.10 fibs" should "construct fibonacci sequence stream" in {
    Stream.fibs.take(4).toList shouldBe List(0, 1, 1, 2)
  }

  "5.11 unfold" should "generate a stream" in {
    Stream.unfold[Int, Int](0)(a => Some(a, a + 1)).take(3).toList shouldBe List(0, 1, 2)
  }

  "5.12 implement fibs, from, constant, ones" should "in term of unfold" in {
    Stream.ViaUnfold.constant(1).take(10).toList shouldBe List.fill(10)(1)
    Stream.ViaUnfold.from(10).take(3).toList shouldBe List(10, 11, 12)
    Stream.ViaUnfold.fibs.take(4).toList shouldBe List(0, 1, 1, 2)
    Stream.ViaUnfold.ones.take(4).toList shouldBe List(1, 1, 1, 1)
  }

  "5.13 unfold map" should "map by unfold" in {
    import Stream.cons
    var count = 0
    val stream = cons({ count += 1; 1}, cons({count += 1; 2}, cons({count+=1; 3}, cons({count+=1; 4}, cons({count+=1; 5}, Stream.empty)))))
    val s = stream.mapViaUnfold(identity)

    count shouldBe(1)
    s.toList shouldBe List(1, 2, 3, 4, 5)
  }

  "5.13 unfold take" should "take n element" in {
    (Stream(1, 2, 3, 4, 5) takeViaUnfold 3).toList shouldBe List(1, 2, 3)
  }

  "5.13 unfold takeWhile" should "take starting element matching predicate" in {
    Stream(1, 2, 3, 2, 1).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)
  }

  "5.13 unfold zipWith" should "zip with function" in {
    Stream(1, 2, 3, 4).zipWith(Stream(4, 3, 2, 1))(_ + _).toList shouldBe List(5, 5, 5, 5)
  }

  "5.13 unfold zipAll" should "zip all element in both stream" in {
    Stream(0, 1).zipAll(Stream(1)).toList shouldBe List((Some(0), Some(1)), (Some(1), None))
  }

  "5.14 startsWith" should "check if one Stream is a prefix of another" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(2, 3)) shouldBe false
  }

  "5.15 tails" should "return all suffixs" in {
    Stream(1, 2, 3, 4).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4))
  }
}
