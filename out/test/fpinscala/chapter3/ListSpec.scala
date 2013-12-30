package chapter3

import org.specs2.mutable.Specification

class ListSpec extends Specification {
  val ls = Cons(1, Cons(2, Cons(3, Nil)))

  "tail" should {
    "return tail of list" in {
      List.tail(ls) must_== Cons(2, Cons(3,Nil))
    }
  }

  "setHead" should {
    "replace head of list" in {
      List.setHead(ls)(5) must_== Cons(5, Cons(2, Cons(3, Nil)))
    }
  }

  "drop" should {
    "remove two elements of list" in {
      List.drop(ls, 2) must_== Cons(3, Nil)
    }
  }

}
