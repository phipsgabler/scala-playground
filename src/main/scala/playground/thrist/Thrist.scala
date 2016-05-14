package playground.thrist

import scala.language.higherKinds

trait Composition[F[_, _], G[_, _], X] {
  def apply[I, J](f: F[I, J], g: G[J, X]): G[I, X]
}


sealed trait Thrist[F[_,_], A, B] {
  def isNil: Boolean

  // (forall i j. (i `arr` j) -> (j `brr` c) -> i `brr` c) -> (b `brr` c) -> Thrist arr a b -> a `brr` c
  def foldRight[X, R[_,_]](f: Composition[F, R, X])(nil: R[B, X]): R[A, X]
}

case class TCons[F[_,_], A, B, C](head: F[A, B], tail: Thrist[F, B, C]) extends Thrist[F, A, C] {
  val isNil = false

  def foldRight[X, R[_,_]](f: Composition[F, R, X])(nil: R[C, X]): R[A, X] = {
    f(head, tail.foldRight(f)(nil))
  }
}

case class TNil[F[_, _], A]() extends Thrist[F, A, A] {
  val isNil = true
  def foldRight[X, R[_,_]](f: Composition[F, R, X])(nil: R[A, X]): R[A, X] = nil
}

// val t1: Thrist[Function1, Int, Int] = TCons((x: Int) => 1, TNil())
// val t2: Thrist[Function1, Int, String] = TCons((x: Int) => x + 1, TCons((x: Int) => x.toString, TNil()))

// def comp[X] = new Composition[Function1, Function1, X] {
//   def apply[I, J] (f: Function1[I, J], g: Function1[J, X] ): Function[I, X] = (i: I) => g (f (i) )
// }
// def id[X]: Function1[X, X] = (x: X) => x

// t2.foldRight(comp[String])(id)(23)
// res10: String = 24