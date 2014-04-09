package chapter5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "headOption" should {
    "rerun head" in {
      Stream(1,2,3,4,5).headOption must_== Some(1)
      Stream(4,2,3,4,5).headOption must_== Some(4)

    }
  }

  "exists" should {
    "return true or false matching a Boolean function exists in Stream" in {
      Stream(3,5,2,9,6).exists(_ == 3) must_== true
      Stream(3,5,2,9,6).exists(_ == 8) must_== false
    }
  }

  //exercise 1
  "toList" should {
    "return to List" in {
      Stream(1,2,3,4,5).toList must_== List(1,2,3,4,5)
      Stream(3,2,1).toList must_== List(3,2,1)
    }
  }

  //exercise 2
  "take" should {
    "return first n elements of a Stream" in {
      Stream(1,2,3,4,5).take(3).toList must_== List(1,2,3)
      Stream(4,9,1,6,8).take(2).toList must_== List(4,9)
    }
  }

  "drop" should {
    "return skipping the first n elements of a Stream" in {
      Stream(2,3,4,5,6).drop(3).toList must_== List(5,6)
      Stream(9,3,5,3,4).drop(4).toList must_== List(4)
      Stream(3,4,5,6).drop(4) must_== Empty
    }
  }

  //exercise 3
  "takeWhile" should {
    "return all starting elements of a Stream that match the given predicate" in {
      Stream(3,7,4,8,0).takeWhile(_ < 8).toList must_== List(3,7,4)
    }
  }

  //exercise 4
  "forAll" should {
    "return true all elements in the Stream match a given predicate" in {
      Stream(1,2,3,5,6).forAll(_ < 7) must_== true
      Stream(1,2,3,5,6).forAll(_ < 6) must_== true
    }
  }
  //exercise 5
  "takeWhileViaFoldRight" should {
    "return all starting elements of a Stream that match the given predicate" in {
      Stream(3,7,4,8,0).takeWhileViaFoldRight(_ < 8).toList must_== List(3,7,4)
    }
  }


}
