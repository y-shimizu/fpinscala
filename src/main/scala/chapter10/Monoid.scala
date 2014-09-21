package chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }
  // Exercise 1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // Exercise 2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero: Option[A] = None
  }

  // Exercise 3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 andThen a2 // a1 compose a2 or (i: A) => a1(a2(i))
    def zero: (A) => A = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // Exercise 5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))

  // Exercise 6
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // Exercise 7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) m.zero
    else if (v.length == 1) f(v.head)
    else {
      val (a, b) = v.splitAt(v.length / 2)
      m.op(foldMapV(a, m)(f), foldMapV(b, m)(f))
    }
  }

  // Exercise 9
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val mon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(o1: Option[(Int, Int, Boolean)], o2: Option[(Int, Int, Boolean)]) =
        (o1, o2) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      val zero = None
    }
    foldMapV(ints, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => Stub(a + b)
      case (Part(l, w, r), Stub(a)) => Part(l, w, r + a)
      case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }
    override def zero: WC = Stub("")
  }

  // Exercise 11
  def countWord(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  // Exercise 16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      } }

  // Exercise 17
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    override def zero: (A) => B = a => B.zero
  }

  // Exercise 18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))
}


trait Foldable[F[_]] {
  import Monoid._
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

// Exerceise 12
object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def toList[A](as: IndexedSeq[A]): List[A] = as.toList

  override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def toList[A](as: Stream[A]): List[A] = as.toList

  override def foldMap[A, B](as: Stream[A])(f: (A) => B)(m: Monoid[B]): B = foldLeft(as)(m.zero)((b, a) => m.op(b, f(a)))
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


// Exercise 13
object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

// Exercise 14
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
    case None => z
    case Some(a) => f(z, a)
  }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
    case None => z
    case Some(a) => f(a, z)
  }

  override def toList[A](as: Option[A]): List[A] = as.toList
}





















