package chapter4

import scala.collection.immutable
import scala.Option

sealed trait Option[+A] {

  //Exercise1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  //Exercise1
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  //Exercise1
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  //Exercise1
  def filter(f: A => Boolean): Option[A] = flatMap(a => if(f(a)) Some(a) else None)

  def filter_other_implementation(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }


  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)



}


case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


object Option extends App {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  //Exercise2
//  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => math.pow(xs(0) - m, 2))
  def sequence[A](a: List[Option[A]]): Option[List[A]] =  a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (h2 => sequence(t) map (h2 :: _) )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa =>
      b map     (bb =>
        f(aa, bb)))


}
