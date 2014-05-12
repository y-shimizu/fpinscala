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

  "hasSubsequence" should {
    "aa" in {
      Stream(7,4,2).hasSubsequence(Stream(2)) must_== true
      Stream(7,4,2).hasSubsequence(Stream(7,4)) must_== true
      Stream(7,4,2).hasSubsequence(Stream(4,2)) must_== true
      Stream(7,4,2).hasSubsequence(Stream(7,2)) must_== false
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
      Stream(1,2,3,5,6).forAll(_ < 6) must_== false
    }
  }

  //exercise 5
  "takeWhileViaFoldRight" should {
    "return all starting elements of a Stream that match the given predicate" in {
      Stream(3,7,4,8,0).takeWhileFoldRight(_ < 8).toList must_== List(3,7,4)
    }
  }

//  "headOptionFoldRight" should {
//    "return the first element to Option of a Stream" in {
//      Stream(9,6,4,2,5).headOptionFoldRight must_== Some(9)
//    }
//  }

  //exercise 12
  "fibs" should {
    "return fibonachi" in {
      Stream.fibs.take(8).toList must_== List(0,1,1,2,3,5,8,13)
    }
  }

  "from" should {
    "return infinite Stream from argument number" in {
      Stream.from(3).take(5).toList must_== List(3,4,5,6,7)
    }
  }

  "constant" should {
    "return infinite Stream of argument value" in {
      Stream.constant("a").take(5).toList must_== List("a","a","a","a","a")
    }
  }

  "ones" should {
    "return infinite Stream of Int 1" in {
      Stream.ones.take(5).toList must_== List(1,1,1,1,1)
    }
  }

  //exercise 13
  "mapUnfold" should {
    "return mapped Stream using unfold method" in {
      Stream(1,2,3,4,5).mapUnfold(_ + 1).toList must_== List(2,3,4,5,6)
    }
  }

  "takeUnfold" should {
    "return Stream number of argument value" in {
      Stream(3,7,9,1,7).takeUnfold(3).toList must_== List(3,7,9)
    }
  }
  "takeUnfold2" should {
    "return Stream different implement" in {
      Stream(3).takeUnfold(3).toList must_== List(3)
    }
  }
  "takeWhile2" should {
    "return Stream" in {
      Stream(1,3,6,8,9,0).takeWhileUnfold(_ < 7).toList must_== List(1,3,6)
    }
  }

  "zipWith" should {
    "return Stream" in {
      Stream(1,2,4,6,7).zipWith(Stream(6,4,3,2,1))(_ + _).toList must_== List(7,6,7,8,8)
      Stream(1,2,4,6,7).zipWith(Stream(6,4))(_ + _).toList must_== List(7,6)
    }
  }

  "zipAll" should {
    "return Stream" in {
      Stream(1,2,3).zipAll(Stream(3,5,6)).toList must_== List((Some(1), Some(3)), (Some(2), Some(5)), (Some(3), Some(6)))
      Stream(1,2).zipAll(Stream(3,5,6)).toList must_== List((Some(1), Some(3)), (Some(2), Some(5)), (None, Some(6)))
    }
  }

  //exercise 14
  "startsWith" should {
    "return true if Stream start with Stream.Empty" in {
      Stream(1,2,3,4,5).startsWith(Stream()) must_== true
    }
    "return true if Stream start with argument Stream" in {
      Stream(1,2,3,4,5).startsWith(Stream(1,2)) must_== true
    }
    "return false if Stream do not start with argument Stream" in {
      Stream(1,2,3,4,5).startsWith(Stream(3,2)) must_== false
    }

  }

  //exercise 15
  "tails" should {
    "return Stream[Stream]" in {
      Stream(3,5,7).tails.toList.map(_.toList) must_== List(List(3,5,7), List(5,7), List(7), List())
    }
  }

  "append" should {
    "append argument Stream to Stream" in {
      Stream(2,4,5).append(Stream(8)).toList must_== List(2,4,5,8)
    }
  }

  //exercise 16
  "scanRight" should {
    "reuse intermediate results so that traversing a Stream with n elements always takes time linear in n" in {
      Stream(1,2,3).scanRight(0)(_ + _).toList must_== List(6,5,3,0)
    }
  }





}
