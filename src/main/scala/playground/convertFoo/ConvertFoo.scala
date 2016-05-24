package playground.convertFoo

class ConvertFoo {

//  see: http://stackoverflow.com/q/37401661/1346276
//  data Foo = Foo
//
//  class ConvertFoo a b where
//    convertFoo :: a -> b
//
//  instance (ConvertFoo a Foo, ConvertFoo Foo b) => ConvertFoo a b where
//    convertFoo = convertFoo . (convertFoo :: a -> Foo)
//
//  evil :: Int -> String
//  evil = convertFoo

  case class Foo()

  trait ConvertFoo[A, B] {
    def convertFoo(a: A): B
  }

  implicit def convertFooInstance[A, B](implicit convertAFoo: ConvertFoo[A, Foo],
                                        convertFooB: ConvertFoo[Foo, B]) = new ConvertFoo[A, B] {
    def convertFoo(a: A): B = convertFooB.convertFoo(convertAFoo.convertFoo(a))
  }

//  this doesn't compile:
//  def evil(i: Int): String = convertFooInstance[Int, String].convertFoo(i)

//    [error] ConvertFoo.scala:28: diverging implicit expansion for type ConvertFoo.this.ConvertFoo[Int,ConvertFoo.this.Foo]
//    [error] starting with method convertFooInstance in class ConvertFoo
//    [error]   def evil(i: Int): String = convertFooInstance[Int, String].convertFoo(i)
//    [error]                                                ^
//    [error] one error found



//  class Bar x
//  instance (Bar x, Bar y, Eq (x,y)) => Bar (x,y)

  trait Bar[X]
  trait Eq[X]

  implicit def tupleIsBar[X, Y](implicit barX: Bar[X],
                                barY: Bar[Y],
                                eqXY: Eq[(X, Y)]) = new Bar[(X, Y)] {}

  implicit val intIsBar = new Bar[Int] {}
  implicit val stringIsBar = new Bar[String] {}
  implicit val intIsEq = new Eq[Int] {}
  implicit val stringIsEq = new Eq[String] {}
  implicit def tuplesAreEq[X, Y](implicit eqX: Eq[X], eqY: Eq[Y]) = new Eq[(X, Y)] {}

//  while this does:
  println(s"witness: ${implicitly[Bar[(Int, String)]]}")

//  scala> new ConvertFoo()
//  witness: playground.convertFoo.ConvertFoo$$anon$6@5b744434
//  res0: playground.convertFoo.ConvertFoo = playground.convertFoo.ConvertFoo@18bbea19

}
