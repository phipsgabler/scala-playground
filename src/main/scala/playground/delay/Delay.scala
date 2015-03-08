package playground.delay

class Delay[+T](delayed: Unit => T) {
  private[this] var cached: Option[T] = None

  def force: T = cached match {
    case Some(value) => value
    case None => {
      val value = delayed(())
      cached = Some(value)
      value
    }
  }

  def map[B](f: T => B): Delay[B] = new Delay(delayed andThen f)

  def flatMap[B](f: T => Delay[B]): Delay[B] = new Delay(_ => f(this.force).force)
}
