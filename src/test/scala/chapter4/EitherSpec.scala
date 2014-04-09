package chapter4

import org.specs2.mutable.Specification

class EitherSpec extends Specification {

  "sequence" should {
    "return Option[List[A]] in case of Some" in {
      Either.sequence(List(Right(1), Right(2), Right(3))) must_== Right(List(1,2,3))
    }

    "return Some(Nil) in case of Nil" in {
      Either.sequence(List()) must_== Right(Nil)
    }
  }

}