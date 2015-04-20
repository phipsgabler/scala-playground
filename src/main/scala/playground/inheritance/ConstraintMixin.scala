package playground

import scala.language.higherKinds

object ConstraintMixin {
  trait Class1[T] {
    val default1: T
  }

  trait Class2[T] {
    val default2: T
  }

  trait Base {
    type S
    type S_Constraint[A]


    def generate(implicit witness: S_Constraint[S]): S
  }

  trait Mixin1 { self: Base =>
    type S_Constraint[A] = Class1[A]
    def generate(implicit witness: S_Constraint[S]): S = witness.default1
  }

  trait Mixin2 { self: Base =>
    type S_Constraint[A] = Class2[A]
    def generate(implicit witness: S_Constraint[S]): S = witness.default2
  }

  abstract class Final1 extends  Base {
    type S = Int
  }

  abstract class Final2 extends  Base {
    type S = String
  }

  object Main extends App {
    implicit object intClass1 extends Class1[Int] {
      val default1 = 1
    }

    implicit object intClass2 extends Class2[Int] {
      val default2 = 0
    }

    implicit object stringClass1 extends Class1[String] {
      val default1 = "hallo"
    }

    val test1 = new Final1 with Mixin1
    val test2 = new Final1 with Mixin2
    val test3 = new Final2 with Mixin1

    println(test1.generate)
    println(test2.generate)
    println(test3.generate)

    def main(): Unit = this.main(new Array(0))
  }
}
