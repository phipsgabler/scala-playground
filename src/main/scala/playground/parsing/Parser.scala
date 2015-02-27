package playground.parsing

class Parser[+T] private (private val run : InputStream => Option[(T, InputStream)])
  extends (InputStream => Option[(T, InputStream)]) {

  import playground.parsing.Parser._

  override def apply(s: InputStream): Option[(T, InputStream)] = run(s)

  def point[U >: T](x: => U): Parser[U] = matching {
    case s => (x, s)
  }

  def |[U >: T](other: => Parser[U]): Parser[U] = continueWithOld {
    case (old, None) => other(old)
    case (_, some) => some
  }

  def ~[U](other: => Parser[U]): Parser[(T, U)] = continueWith {
    case Some((x, rest)) => other.run(rest) match {
      case Some((y, rest2)) => Some((x, y), rest2)
      case None => None
    }
  }

  def ~>[U](other: => Parser[U]): Parser[U] = (this ~ other) map {
    case (_, y) => y
  }

  def <~[U](other: => Parser[U]): Parser[T] = (this ~ other) map {
    case (x, _) => x
  }

  def * : Parser[List[T]] = many(this)

  def + : Parser[List[T]] = many1(this)

  def ? : Parser[Option[T]] = optional(this)

  def map[U](f: T => U): Parser[U] = continueWith {
    case r => r map { case (x, rest) => (f(x), rest)}
  }

  def |@|[R >: T, U, V](other: => Parser[U]): Applicator2[R, U, V] = new Applicator2(this, other)
  
  def flatMap[U](f: T => Parser[U]) : Parser[U] = continueWith {
    case Some((x, rest)) => f(x).run(rest)
  }
  
  def withFilter(f: T => Boolean) : Parser[T] = continueWith {
    case r@Some((x, rest)) if f(x) => r
  }

  private[parsing] def continueWith[U](f: PartialFunction[Option[(T, InputStream)], Option[(U, InputStream)]])
  : Parser[U] = Parser[U]((s: InputStream) => run(s) match {
    case r if f.isDefinedAt(r) => f(r)
    case _ => None
  })

  private[parsing] def continueWithOld[U](f: PartialFunction[(InputStream, Option[(T, InputStream)]), Option[(U, InputStream)]])
  : Parser[U] = Parser[U]((s: InputStream) => run(s) match {
    case r if f.isDefinedAt((s, r)) => f(s, r)
    case _ => None
  })
}

object Parser {
  object %:: {
    def unapply(xs: InputStream): Option[(Char, InputStream )] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }


  private[parsing] class Applicator2[A, B, C](val p: Parser[A], val q: Parser[B]) {
    def apply(f: (A, B) => C): Parser[C] = (p ~ q) map (t => f(t._1, t._2))

    def |@|[D](other: Parser[C]) = new Applicator3[A, B, C, D](p, q, other)
  }
  private[parsing] class Applicator3[A, B, C, D](val p: Parser[A], val q: Parser[B], val r: Parser[C]) {
    def apply(f: (A, B, C) => D): Parser[D] =
      (p ~ q ~ r) map (t => f(t._1._1, t._1._2, t._2))
  }

  def apply[T](f: InputStream => Option[(T, InputStream )]) = new Parser[T](f)

  def satisfy(p: Char => Boolean) : Parser[Char] = matching {
    case c %:: rest if p(c) => (c, rest)
  }

  implicit def character(char: Char) : Parser[Char] = matching {
    case `char` %:: rest => (char, rest)
  }

  def matching[T](f : PartialFunction[InputStream , (T, InputStream )]) : Parser[T] = Parser(f.lift)

  def many[T](p: => Parser[T]) : Parser[List[T]] = p.continueWithOld {
    case (_, Some((x, rest))) => {
      val tail = many(p).run(rest).getOrElse((Nil, rest))
      Some((x::tail._1, tail._2))
    }
    case (old, None) => Some(Nil, old)
  }

  def optional[T](p: => Parser[T]) : Parser[Option[T]] = p.continueWithOld {
    case (_, Some((x, rest))) => Some((Some(x), rest))
    case (old, None) => Some((None, old))
  }

  def many1[T](p: => Parser[T]) : Parser[List[T]] = (p |@| many(p))(_::_)
}