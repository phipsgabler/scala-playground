package playground.inheritance

import scala.language.higherKinds

object MixinParameter {

  /** Type class describing the trivial way of generating values. */
  trait Class1[T] {
    val default: T
  }

  /** Type class describing a more comples way of generating T's.
    * (In reality, this is Scalacheck's Arbitrary.)
    */
  trait Class2[T] {
    val gen: Gen[T]
  }

  trait Gen[T] {
    def generate: T
  }

  /** Base trait. Can generate (run) values of a type S; how that is (structurally) done, is described by
    * subclasses; but the generation of individual values might vary and is mixed in.
    */
  trait Base {
    type S
    type S_Constraint[A]

    def generate(implicit witness: S_Constraint[S]): S
    def run(implicit witness: S_Constraint[S]): Stream[S] = generate #:: run
  }


  /** Trivial generation, requiring Class1 for default values */
  trait Mixin1 { self: Base =>
    type S_Constraint[A] = Class1[A]
    def generate(implicit witness: S_Constraint[S]): S = witness.default
  }

  /** More complex generation, which takes the Gen from Class2 to generate values.
    * But it should be possible to replace the default Gen with a different one, using withParameter.
    */
  trait Mixin2 { self: Base =>
    type S_Constraint[A] = Class2[A]

    protected def generator(implicit witness: S_Constraint[S]): Gen[S] = witness.gen

    def generate(implicit witness: S_Constraint[S]) = generator.generate

    type MyType = Base with Mixin2 { type S = self.S; type S_Constraint[A] = self.S_Constraint[A] }

    /** This is the problematic part. Should be able to override the default generation behaviour,
      * or to give it additional parameters.
      */
//    def withParameter(param: Gen[S]): Base with Mixin2 = new Base with Mixin2 {
//      type S = self.S
//      override protected def generator(implicit witness: S_Constraint[S]): Gen[S] = param
////                                                                does not compile: ^^^^^
////      [error]  found   : MixinParameter.Gen[Mixin2.this.S]
////      [error]  required: MixinParameter.Gen[this.S]
//    }

//    def withParameter(param: Gen[S]): self.type = new self.type {
//      override protected def generator(implicit witness: S_Constraint[S]): Gen[S] = param
//    }
  }
}

//object MixinParameterTest {
//  import MixinParameter._
//
//  implicit object intIsClass1 extends Class1[Int] {
//    val default = 0
//  }
//
//  implicit object intIsClass2 extends Class2[Int] {
//    val gen = new Gen[Int] {
//      def generate = 42
//    }
//  }
//
//  /** One almost-implementation of Base; this would only define the structure of how "run"
//    * works, the actual generation of Ints is still to be mixed in.
//    */
//  abstract class Final extends Base {
//    type S = Int
//  }
//
//  val bar = new Final with Mixin1 // trivial generation
//
//  val param = new Gen[Int] { def generate = 42 } // we want a more "complex" instance than the default one.
//  val foo = new Final with Mixin2 withParameter param
//
//  def test() = println(foo.generate)
//}
