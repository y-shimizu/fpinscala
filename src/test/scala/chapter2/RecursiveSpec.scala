package chapter2

import org.specs2.mutable.SpecificationWithJUnit


class RecursiveSpec extends SpecificationWithJUnit {
  //Exercise 1
  "fib" should {
    "return fibonacci sequence" in {
      Recursive.fib(0) must_== 0
      Recursive.fib(1) must_== 1
      Recursive.fib(7) must_== 13
    }
  }

  //Exercise 2
  "isSorted" should {
    "return true or false" in {
      Recursive.isSorted(Array(1,4,5,6,9), (a: Int, b: Int) => a < b) must_== true
      Recursive.isSorted(Array(1,4,2,6,3), (a: Int, b: Int) => a < b) must_== false
    }
  }

}
