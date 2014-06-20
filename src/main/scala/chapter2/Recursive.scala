package chapter2

object Recursive extends App {
  //Exercise 1
  def fib(n: Int): Int = {
    def go(n1: Int, n2: Int, count: Int): Int = {
      if (count == 0) n1
      else if (count == 1) n2
      else go(n2, n1 + n2, count -1)
    }
    go(0, 1, n)
  }
}
