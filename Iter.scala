import scala.io.Source
import java.io.RandomAccessFile

trait Iter[T] extends Iterator[T] {
  def rewind(position: Long): Unit

  def position(): Long

  def span1(p: T => Boolean): (Iter[T], Iter[T]) = ???
}

class RAIter(filename: String) extends Iter[Char] {

  private val _raf = RandomAccessFile(filename, "r")
  private var l2Read = _raf.length()

  override def hasNext: Boolean = l2Read == 0

  override def next(): Char =
    l2Read -= 1
    _raf.readChar()

  override def rewind(position: Long): Unit =
    _raf.seek(position)

  override def position(): Long = _raf.length() - l2Read

}
