import scala.io.Source
import java.io.RandomAccessFile
import java.{util => ju}

trait Iter[T] extends Iterator[T] {
  def rewind(position: Long): Unit

  def position: Long

  def span1(p: T => Boolean): (Iter[T], Iter[T])
}

class RAIter(filename: String) extends Iter[Char] {

  override def span1(p: Char => Boolean): (Iter[Char], Iter[Char]) = ???

  private val _raf = RandomAccessFile(filename, "r")
  private var l2Read = _raf.length()

  override def hasNext: Boolean = l2Read != 0

  override def next(): Char =
    l2Read -= 1
    _raf.readChar()

  override def rewind(position: Long): Unit =
    _raf.seek(position)

  override def position: Long = _raf.length() - l2Read

}

class ArrayIter[T](arr: T*) extends Iter[T] {

  private var _l2Read: Long = arr.length

  override def span1(p: T => Boolean): (Iter[T], Iter[T]) =
    var (a, b) = arr.slice(position.toInt, arr.length).span(p)
    (ArrayIter(a: _*), ArrayIter(b: _*))

  override def hasNext: Boolean = _l2Read != 0

  override def next(): T =
    val r = arr(position.toInt)
    _l2Read -= 1
    r

  override def rewind(position: Long): Unit =
    _l2Read = arr.length - position

  override def position: Long = arr.length - _l2Read
}
