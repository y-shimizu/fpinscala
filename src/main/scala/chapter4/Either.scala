package chapter4


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => Right(a)
    case Left(_) => b
  }
  //  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception,Double] =
    for {
      a <- Try {println("hello1"); age.toInt }
      tickets <- Try {println("hello2"); numberOfSpeedingTickets.toInt }
    } yield insuranceRateQuote(a, tickets)
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 0.0

  def traverse[A, B, E](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    a match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequence[A, E](a: List[Either[E, A]]): Either[E ,List[A]] =
  traverse(a)(identity)
}

object Run extends App {
  println(Either.parseInsuranceRateQuote("aaa", "bbb"))

}
