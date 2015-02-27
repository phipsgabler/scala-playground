package playground.operational

import scala.language.existentials
import scala.language.higherKinds

sealed trait Program[I[+_], +A]{
  import Program._

  def map[B](f: A => B) : Program[I, B] = Bind(this, (v : A) => Lift(f(v)))
  def flatMap[B](f: A => Program[I, B]) : Program[I, B] = Bind(this, f)

  def view[T >: A]: ProgramView[I, T]
}

case class Lift[I[+_], +A](value: A) extends Program[I, A] {
  import Program._

  def view[T >: A]: ProgramView[I, T] = Ret(value)
}

case class Instr[I[+_], +A](instr: I[A]) extends Program[I, A] {
  import Program._

  def view[T >: A]: ProgramView[I, T] = Bnd[I, A, A](instr, (x: A) => Lift[I, A](x))
}

case class Bind[I[+_], -X, +A](prog: Program[I, X], f: X => Program[I, A]) extends Program[I, A] {
  import Program._

  def view[T >: A]: ProgramView[I, T] = prog match {
    case Lift(v) => f(v).view
    case Instr(i) => Bnd[I, X, A](i, f)
    case bound: Bind[I, m, X] => Bind[I, m, A](bound.prog, (x: m) => Bind[I, X, A](bound.f(x), f)).view
  }
}


object Program {
  def value[I[+_], A](x: A) : Program[I, A] = Lift(x)
  def singleton[I[+_], A](i: I[A]) : Program[I, A] = Instr(i)
  def <*>[I[+_], A, B](pf: Program[I, A => B], pa: Program[I, A]) : Program[I, B] = for {
    f <- pf
    a <- pa
  } yield f(a)

  private[operational] sealed trait ProgramView[I[+_], +A]
  private[operational] case class Ret[I[+_], +A](x: A) extends ProgramView[I, A]
  private[operational] case class Bnd[I[+_], X, +A](i: I[X], f: X => Program[I, A]) extends ProgramView[I, A]

  object >>= {
    def unapply[I[+_], A](p: Program[I, A]): Option[(I[X], X => Program[I, A]) forSome {type X}] = p.view match {
      case bound: Bnd[I, x, A] => Some[(I[x], x => Program[I, A])]((bound.i, bound.f))
      case _ => None
    }
  }

  object Return {
    def unapply[I[+_], A](p: Program[I, A]): Option[A] = p.view match {
      case Ret(x) => Some(x)
      case _ => None
    }
  }
}








