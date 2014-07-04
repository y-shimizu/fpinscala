package chapter2

object Recursive {
  //Exercise 1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n1: Int, n2: Int, count: Int): Int = {
      if (count == 0) n1
      else go(n2, n1 + n2, count -1)
    }
    go(0, 1, n)
  }

  //Exercise 2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n < 1) true
      else gt(as(n - 1), as(n)) && go(n - 1)
    }
    go(as.length - 1)
  }

  //Exercise 3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

  //Exercise 4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  //Exercise 5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

}
