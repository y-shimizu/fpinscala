def fib(n: Int): BigInt = if (n < 2) n else fib(n - 2) + fib(n - 1)

fib(10)
