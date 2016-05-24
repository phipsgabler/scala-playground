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

  def evil(i: Int): String = convertFooInstance[Int, String].convertFoo(i)

//    [error] ConvertFoo.scala:28: diverging implicit expansion for type ConvertFoo.this.ConvertFoo[Int,ConvertFoo.this.Foo]
//    [error] starting with method convertFooInstance in class ConvertFoo
//    [error]   def evil(i: Int): String = convertFooInstance[Int, String].convertFoo(i)
//    [error]                                                ^
//    [error] one error found


}
