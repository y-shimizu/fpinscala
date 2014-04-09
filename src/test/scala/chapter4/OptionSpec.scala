package chapter4

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "map" should {
    "return Some or None" in {
      Some(2).map(_ * 2) must_== Some(4)
      None.map((a: Int) => a * 2) must_== None
    }
  }

  "flatMap" should {
    "return Some or None" in {
      Some(3).flatMap((a:Int) => Some(a * 2)) must_== Some(6)
      None.flatMap((a:Int) => Some(a * 2)) must_== None
    }
  }

  "getOrElse" should {
    "return value in case of Some" in {
      val some: Any = Some(3).getOrElse("aa")
      println()
      println(some.getClass)
      println()
      Some(3).getOrElse("aa") must_== 3

    }
    "return argument in case of None" in {
      None.getOrElse("aa") must_== "aa"
    }
  }
  "orElse" should {
    "return argument in case of None" in {
      None.orElse(Some(0)) must_== Some(0)
    }
    "return itself in case of Some" in {
      Some(4).orElse(Some(0)) must_== Some(4)
    }
  }
  "filter" should {
    "return " in {
      Some(13).filter(_ > 10) must_== Some(13)
      Some(9).filter(_ > 10) must_== None
      None.filter((a: Int) => a > 10) must_== None
    }
  }

  "sequence" should {
    "return Option[List[A]] in case of Some" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) must_== Some(List(1,2,3))
    }
    "return None in case of containing None" in {
      Option.sequence(List(Some(1), Some(2), None)) must_== None
    }
    "return None in case of containing None in forward" in {
      Option.sequence(List(None,Some(1), Some(2))) must_== None
    }
    "return Some(Nil) in case of Nil" in {
      Option.sequence(List()) must_== Some(Nil)
    }
  }



}
