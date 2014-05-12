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
  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  /**
   * exercise 6
   */
  //  def headOptionFoldRight: Option[A] = foldRight(this)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)(Stream.cons(_, _))

  //ex 13
  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold(this,n) {
    case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case Cons(h, t) => None
    case Empty => None
  }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold(this, other) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case (_, Empty) => None
    case (Empty, _) => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold(this, s2) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Empty, Empty) => None
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    a => a match {
      case Cons(h, t) => Some(a, t())
      case Empty => None
    }
  }.append(Stream(Empty))

  def tails2: Stream[Stream[A]] = Stream.unfold(this) {
    case s@Cons(_, t) => Some(s, t())
    case Empty => None
  }

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() =>  t1().startsWith(t2())
    case _ => false
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z)) { (a, b) =>
      Stream.cons(f(a, b.headOption.get), b)
    }

//  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
//    case Cons(h,t) => f(h(), t().foldRight(z)(f))
//    case _ => z
//  }



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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a,unfold(s)(f))
      case None => empty
    }

  def fibs: Stream[Int] = unfold(0, 1)(s => Some((s._1, (s._2, s._1 + s._2))))

  def from(a: Int) = unfold(a)(i => Some(i, i + 1))

  def constant[A](a: A) = unfold(a)(i => Some(i, i))

  def ones = constant(1)
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