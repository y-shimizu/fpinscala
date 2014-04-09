package chapter5

/* 4/5 */

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

//  def exists(p: A => Boolean): Boolean = this match {
//    case Cons(h, t) => p(h()) || t().exists(p)
//    case _ => false
//  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  /**
   * exercise 1
   * toList
   */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
  //  def toList: List[A] =  {
  //    def f(s: Stream[A]): List[A] = s match {
  //      case Empty => Nil
  //      case Cons(h,t) => h() :: f(t())
  //    }
  //    f(this)
  //  }

  /**
   * exercise 2
   * take, drop
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }
  //  def take(n: Int): Stream[A] = {
  //    def f(s: Stream[A], count: Int): Stream[A] = s match {
  //      case Empty => Empty
  //      case Cons(_, _) if count > n => Empty
  //      case Cons(h,t) => Stream.cons(h(), f(t(), count + 1))
  //    }
  //    f(this, 1)
  //  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => Stream.cons(h(), t())
    case _ => Stream.empty
  }

  /**
   * exercise 3
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  /**
   * exercise 4
   */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
   * exercise 5
   */
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}








































//sealed trait Stream[+A] {
//  def toList: List[A] = {
//    def f(s: Stream[A]): List[A] = s match {
//      case Empty => Nil
//      case Cons(h, t) => h() :: f(t())
//    }
//    f(this)
//  }
//
//  def take(n: Int): Stream[A] = {
//    def f(s: Stream[A], count: Int): Stream[A] = s match {
//      case Empty => Empty
//      case Cons(_, _) if count > n => Empty
//      case Cons(h, t) => Stream.cons(h(), f(t(), count + 1))
//    }
//    f(this, 1)
//  }
//
//  def drop(n: Int): Stream[A] = {
//    def f(s: Stream[A], count: Int): Stream[A] = s match {
//      case Empty => Empty
//      case Cons(_, t) if count < n + 1 => f(t(), count + 1)
//      case Cons(h, t) => Stream.cons(h(), f(t(), count + 1))
//    }
//    f(this, 1)
//  }
//
//  def takeWhile(p: A => Boolean): Stream[A] = {
//    def f(s: Stream[A]): Stream[A] = s match {
//      case Empty => Empty
//      case Cons(h, t) if p(h()) => Stream.cons(h(),f(t()))
//      case Cons(_, _) => Empty
//    }
//    f(this)
//  }
//
//  def exists(p: A => Boolean): Boolean = this match {
//    case Empty => false
//    case Cons(h, t) => p(h()) || t().exists(p)
//  }
//
//  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
//    case Empty => z
//    case Cons(h,t) => f(h(), t().foldRight(z)(f))
//  }
//
//  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
//
//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => Some(h())
//  }
//
////  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = Empty
//
//  def headOptionUseFoldRight: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))
//
////  def map[B](f: A => B): Stream[B] = foldRight(Empty)((h,t) => Stream.cons(f(h),t))
//}
//case object Empty extends Stream[Nothing]
//case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
//object Stream {
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//    lazy val head = hd
//    lazy val tail = tl
//    Cons(() => head, () => tail)
//  }
//  def empty[A]: Stream[A] = Empty
//  def apply[A](as: A*): Stream[A] =
//    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
//
//}



































































//sealed trait Stream[+A] {
//  def exists(p: A => Boolean): Boolean = this match {
//    case Cons(h, t) => p(h()) || t().exists(p)
//    case _ => false
//  }
//
//  def headOption: Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => Some(h())
//  }
//
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//    lazy val head = hd
//    lazy val tail = tl
//    Cons(() => head, () => tail)
//  }
//  def foldRight[B](z: => B)(f: (A, => B) => B): B =
//    this match {
//      case Cons(h,t) => f(h(), t().foldRight(z)(f))
//      case _ => z }
//
//  def headOptionFoldRight: Option[A] = {
//    foldRight(None: Option[A]) {
//      case (a, _) => Some(a)
//    }
//  }
//
////  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))
//}
//
//case object Empty extends Stream[Nothing]
//case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
//object Stream {
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//    lazy val head = hd
//    lazy val tail = tl
//    Cons(() => head, () => tail)
//  }
//  def empty[A]: Stream[A] = Empty
//  def apply[A](as: A*): Stream[A] =
//    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
//
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//    lazy val head = hd
//    lazy val tail = tl
//    Cons(() => head, () => tail)
//  }
//
//
//
//
//}