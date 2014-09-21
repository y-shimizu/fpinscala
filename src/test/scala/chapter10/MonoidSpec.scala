package chapter10

import org.specs2.mutable.SpecificationWithJUnit
import chapter10.Monoid.{Stub, Part}


class MonoidSpec extends SpecificationWithJUnit {

  "intAddition" should {
    import Monoid.intAddition
    "be implemented op and zero" in {
      intAddition.op(5,7) must_== 12
      intAddition.zero must_== 0
    }
  }

  "intMultiplication" should {
    import Monoid.intMultiplication
    "be implemented op and zero" in {
      intMultiplication.op(3,5) must_== 15
      intMultiplication.zero must_== 1
    }
  }

  "booleanOr" should {
    import Monoid.booleanOr
    "be implemented op and zero" in {
      booleanOr.op(true, false) must beTrue
      booleanOr.op(true, true) must beTrue
      booleanOr.op(false, false) must beFalse
      booleanOr.zero must_== false
    }
  }

  "booleanAnd" should {
    import Monoid.booleanAnd
    "be implemented op and zero" in {
      booleanAnd.op(true, false) must beFalse
      booleanAnd.op(true, true) must beTrue
      booleanAnd.op(false, false) must beFalse
      booleanAnd.zero must_== true
    }
  }

  // Exercise 2
  "optionMonoid" should {
    import Monoid.optionMonoid
    "be implemented op and zero" in {
      optionMonoid.op(Some(3), Some(8)) must_== Some(3)
      optionMonoid.zero must_== None
    }
  }

  "concatenate" should {
    import Monoid.intAddition
    import Monoid.stringMonoid
    "concat elements of List" in {
      Monoid.concatenate(List(1,2,3,4,5), intAddition) must_== 15
      Monoid.concatenate(List("a", "b", "c"), stringMonoid) must_== "abc"
    }
  }

  // Exercise 5
  "foldMap" should {
    import Monoid.intAddition
    "fold and map elements of List " in {
      Monoid.foldMap(List(1,2,3), intAddition)((a: Int) => a + 2) must_== 12
    }
  }

  // Exercise 6
  "foldLeft and foldRight" should {
    "fold List" in {
      Monoid.foldLeft(List(1,2,3,4,5))(1)(_ * _) must_== 120
      Monoid.foldRight(List(2,4,6,8))(1)(_ * _) must_== 384
    }
  }

  // Exercise 7
  "foldMapIndexedSeq" should {
    import Monoid.intMultiplication

    "return fold value when toInt" in {
      Monoid.foldMapV(IndexedSeq("4" , "3" , "10"), intMultiplication)(x => x.toInt) must_== 120
    }

    "return fold value when * 2" in {
      Monoid.foldMapV(IndexedSeq(4 , 3 , 10), intMultiplication)(_ * 2) must_== 960
    }
  }

  // Exercise 9
  "ordered" should {

    "return true when IndexedSeq is sorted" in {
      Monoid.ordered(IndexedSeq(1,2,3,4,5)) must beTrue
      Monoid.ordered(IndexedSeq(1,2,3,5,4)) must beFalse
    }
  }
  // Exercise 10
  "wcMonoid" should {
    import chapter10.Monoid.wcMonoid._
    "be implemented op, zero" in {
      op(Part("aaa", 0, "bbb"), Part("ccc", 0, "ddd")) must_== Part("aaa", 1, "ddd")
      op(Stub("aaa"), Stub("bbb")) must_== Stub("aaabbb")
      op(Part("aaa", 0, "bbb"), Stub("ccc")) must_== Part("aaa", 0, "bbbccc")
      op(Stub("aaa"), Part("bbb", 0, "ccc")) must_== Part("aaabbb", 0, "ccc")
    }
  }

  // Exercise 11
  "countWord" should {
    "count word" in {
      Monoid.countWord("i am adam") must_== 3
    }
  }

  // Exercise 16
  "productMonoid" should {
    import Monoid.{intAddition, stringMonoid}
    "be implement op and zero" in {
      Monoid.productMonoid(intAddition, stringMonoid).op((3, "aaa"), (5, "bbb")) must_== (8, "aaabbb")
    }
  }


  // Exercise 18
  "bag" should {
    "count word and set Map to key-value" in {
      Monoid.bag(Vector("a", "rose", "is", "a", "rose")) must havePairs("a" -> 2, "rose" -> 2, "is" -> 1)
    }
  }






  //  "intAddition" should {
  //    import chapter10.Monoid.intAddition._
  //    "be here" in {
  //      op(1, 4) must_== 5
  //      zero must_== 0
  //    }
  //
  //  }
  //
  //  "intMultiplication" should {
  //    import chapter10.Monoid.intMultiplication._
  //    "be here" in {
  //      op(2, 4) must_== 8
  //      zero must_== 1
  //    }
  //  }

  //  "foldMap" should {
  //    "return fold value when toInt" in {
  //      foldMap(List("4" , "3" , "10"), intMultiplication)(x => x.toInt) must_== 120
  //    }
  //    "return fold value when * 2" in {
  //      foldMap(List(4 , 3 , 10), intMultiplication)(_ * 2) must_== 960
  //    }
  //  }
  //
  //  "foldMapIndexedSeq" should {
  //    "return fold value when toInt" in {
  //      foldMapV(IndexedSeq("4" , "3" , "10"), intMultiplication)(x => x.toInt) must_== 120
  //    }
  //    "return fold value when * 2" in {
  //      foldMapV(IndexedSeq(4 , 3 , 10), intMultiplication)(_ * 2) must_== 960
  //    }
  //  }
  //
  //  "wcMonoid" should {
  //    import chapter10.Monoid.wcMonoid._
  //    "be implemented op, zero" in {
  //      op(Part("a", 0, "b"), Part("c", 0, "d")) must_== Part("a", 1, "d")
  //      op(Stub("aaa"), Stub("bbb")) must_== Stub("aaabbb")
  //      op(Part("a", 0, "b"), Stub("c")) must_== Part("a", 0, "bc")
  //      op(Stub("a"), Part("b", 0, "c")) must_== Part("ab", 0, "c")
  //
  //    }
  //  }

  //  "Foldable[List]" should {
  //    "implement foldRight" in {
  //      import chapter10.Foldable._
  //
  //
  //    }
  //  }



}

