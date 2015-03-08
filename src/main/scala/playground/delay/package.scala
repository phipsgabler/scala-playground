package playground


package object delay {
  def delay[T](value: => T): Delay[T] = new Delay(_ => value)
}