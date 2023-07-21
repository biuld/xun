import scala.util.Try
import scala.util.Failure
import scala.util.Success

class SourcePos(col: Long, row: Long)
class SourceRange(srcStart: SourcePos, srcEnd: SourcePos)
class SourceToken[T](tokVal: T, tokRange: SourceRange)

case class Parser[S, A](unParser: Iter[S] => (Try[A], Iter[S])) {

  def run(input: Iter[S]): (Try[A], Iter[S]) = unParser(input)

  def eval(input: Iter[S]): Try[A] = run(input)._1

  def flatMap[B](f: A => Parser[S, B]): Parser[S, B] =
    Parser(input =>
      val (r, s) = unParser(input)
      r match
        case Failure(ex) => (Failure(ex), s)
        case Success(a)  => f(a).run(s)
    )

  def map[B](f: A => B): Parser[S, B] =
    Parser(input =>
      val (r, s) = unParser(input)
      r match
        case Failure(ex) => (Failure(ex), s)
        case Success(a)  => (Success(f(a)), s)
    )

  def withFilter(p: A => Boolean): Parser[S, A] =
    Parser(input =>
      val (r, s) = unParser(input)
      r match
        case Failure(ex) => (Failure(ex), s)
        case Success(a) =>
          if p(a) then (Success(a), s)
          else (Failure(ParserExp("predicate failed")), s)
    )

  infix def <|>(p: Parser[S, A]): Parser[S, A] = Parser(input =>
    val (r, s) = unParser(input)
    r match
      case Failure(exception) => p.unParser(input)
      case Success(value)     => (r, s)
  )
}

case class ParserExp(txt: String) extends Exception {
  override def toString(): String = s"ParserExp: ${txt}"
}

object Parser {
  def attempt[S, A](p: Parser[S, A]): Parser[S, A] = Parser(input =>
    val pos = input.position
    val (r, s) = p.unParser(input)
    r match
      case Failure(exception) => input.rewind(pos); (r, s)
      case Success(value)     => (r, s)
  )

  def get[S]:Parser[S, Iter[S]] = Parser(i => (Success(i), i))

  def munch[T]: Parser[T, T] = Parser(input =>
    input.nextOption() match
      case None => (Failure(ParserExp("the iterator has been consumed")), input)
      case Some(value) => (Success(value), input)
  )

  def lexeme[S, A](spaceEater: Parser[S, Unit], m: Parser[S, A]): Parser[S, A] =
    for
      r <- m
      _ <- spaceEater
    yield r

  def symbol[T](s: T): Parser[T, T] = munch.withFilter(_ == s)

  def span[T](p: T => Boolean): Parser[T, Iter[T]] = Parser(input =>
    val (r, s) = input.span1(p)
    (Success(r), s)
  )

  def int: Parser[Char, Int] =
    for i <- span[Char](_.isDigit)
    yield i.foldLeft("")(_ + _).toInt

  def long: Parser[Char, Long] =
    for i <- span[Char](_.isDigit)
    yield i.foldLeft("")(_ + _).toLong

  def float: Parser[Char, Float] =
    for
      d <- span[Char](_.isDigit)
      dstr = d.foldLeft("")(_ + _)
      _ <- symbol('.')
      f <- span[Char](_.isDigit)
      fstr = f.foldLeft("")(_ + _)
    yield s"$dstr.$fstr".toFloat

  def double: Parser[Char, Double] =
    for
      d <- span[Char](_.isDigit)
      dstr = d.foldLeft("")(_ + _)
      _ <- symbol('.')
      f <- span[Char](_.isDigit)
      fstr = f.foldLeft("")(_ + _)
    yield s"$dstr.$fstr".toDouble
}

@main def main(): Unit =
  val input: Iter[Char] = ArrayIter('1', '2', '.', '4')
  // val p = for
  //   a <- Parser.long
  //   _ <- Parser.symbol('a')
  //   b <- Parser.int
  // yield a + b
  println(Parser.float.eval(input))
