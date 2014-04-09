package chapter4


//sealed trait Option[+A] {
//  def map[B](f: A => B): Option[B] = this match {
//    case None => None
//    case Some(a) => Some(f(a))
//  }
//
//  def flatMap[B](f: A => Option[B]): Option[B] = {
//    map(f).getOrElse(None)
//  }
//
//  def getOrElse[B >: A](default: => B): B = this match {
//    case None => default
//    case Some(a) => a
//  }
//
//  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
//    case None => ob
//    case _ => this
//  }
//
//  def filter(f: A => Boolean): Option[A] = this match {
//    case _ => None
//    case Some(a) if f(a) => this
//  }
//
//  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
//}
//case class Some[+A](get: A) extends Option[A]
//case object None extends Option[Nothing]
//

object Lean extends App {


}
