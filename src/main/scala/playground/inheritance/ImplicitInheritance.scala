package playground.inheritance


/**
 * How well does inheritance of implicit wrappers work?
 * well enough, it seems.
 */
object ImplicitInheritance {
  trait Basic {
    type T
  }

  trait Foo { self: Basic =>
    trait X {}
    trait Y {}

    implicit class XAndYWrapper(wrapped: X with Y) {
      def foo[T] = "foo"
    }

    implicit class XWrapper(wrapped: X) {
      def foo[T] = "not foo"
    }

    val xy = new X with Y {}
    val x = new X {}
  }

  trait SubBar extends Basic with Foo

  abstract class TestBar extends SubBar {
    println(x.foo)
    println(xy.foo)
  }
}


