package chapter2

import org.specs2.mutable.Specification


class RecursiveSpec extends Specification {
  //Exercise 1
  "fib" should {
    "return fibonacci sequence" in {
      Recursive.fib(0) must_== 0
      Recursive.fib(1) must_== 1
      Recursive.fib(7) must_== 13
    }
  }

}
