import scala.util.Try
import scala.util.Failure
import scala.util.Success

class SourcePos(col: Long, row: Long)
class SourceRange(srcStart: SourcePos, srcEnd: SourcePos)
class SourceToken[T](tokVal: T, tokRange: SourceRange)

class Parser[S, A](unParser: Iterator[S] => (Try[A], Iterator[S])) {

  def run(input: Iterator[S]): (Try[A], Iterator[S]) = unParser(input)

  def eval(input: Iterator[S]): Try[A] = run(input)._1

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
}

case class ParserExp(txt: String) extends Exception {
  override def toString(): String = s"ParserExp: ${txt}"
}

object Parser {
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

  def span[T](p: T => Boolean): Parser[T, Iterator[T]] = Parser(input =>
    val (r, s) = input.span(p)
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

@main def main() =
  val input = Iterator('1', '2', '3', '.', '4')
  val p = for
    a <- Parser.long
    _ <- Parser.symbol('a')
    b <- Parser.int
  yield a + b
  println(Parser.float.eval(input))
