import playground.parsing.Parser
import playground.parsing.Parser._
import playground.operational._
import playground.operational.Program._
import playground.delay.{delay, Delay}

object Main extends Operational {
  private def interact(i: String => Unit) = {
    io.Source.stdin.getLines().foreach(i)
  }

  def main(args : Array[String]): Unit = {
    //interact { input => println(parser(input.toStream)) }

//    val x = interpret(for {
//      _ <- push(1)
//      x <- pop
//      y <- pop
//    } yield x + y)(List(1, 2, 3))
//
//    println(s"Result: $x")
  }
}

trait Delayed {
  val x: Delay[Int] = delay {
    x.force + 1
  }

  val y: Delay[Int] = for {
    v <- y
  } yield v + 1
}

trait Operational {
//  sealed trait StackInstruction[+T]
//  case class Push(i: Int) extends StackInstruction[Unit]
//  case object Pop extends StackInstruction[Int]
//
//  def push(i: Int) = Instr(Push(i))
//  def pop() = Instr(Pop)
//
//  type StackProgram[T] = Program[StackInstruction, T]
//  type Stack[V] = List[V]
//
//  @annotation.tailrec
//  def interpret[A](stackProgram: StackProgram[A])(stack: Stack[Int]): A = stackProgram match {
//    case Return(v) => v
//    case Push(a) >>= instr => interpret[Unit](instr(()))(a::stack)
//    case Pop >>= instr => interpret[Int](instr(stack.head))(stack.tail)
//  }
}

trait Parsing {
  val parser1 = (character('a') | 'b') ~> many(matching { case c %:: cs if c.isUpper => (c, cs) })
  val parser2 = (many1('a') |@| 'X')((xs, x) => x::xs)
}

