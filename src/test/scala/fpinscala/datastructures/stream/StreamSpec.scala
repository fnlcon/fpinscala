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
}
