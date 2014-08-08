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

  //Exercise 3
  "curry" should {
    "return curry function" in {
      val f = (a: Int) => (b: Double) => a * b
      val g = (a: Int, b: Double) => a * b
      Recursive.curry(g)(1)(2) must_== f(1)(2)
    }
  }

  //Exercise 4
  "uncurry" should {
    "return uncurry function" in {
      val f = (a: Int) => (b: Double) => a * b
      val g = (a: Int, b: Double) => a * b
      Recursive.uncurry(f)(1, 2.0) must_== g(1, 2.0)
    }
  }

  //Exercise 5
  "compose" should {
    "return uncurry function" in {
      val f = (a: Double) => a.toString
      val g = (a: Int) => a.toDouble
      Recursive.compose(f, g)(3) must_== f(g(3))
    }
  }

}
