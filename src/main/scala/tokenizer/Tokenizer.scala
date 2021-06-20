package c.y.z
package tokenizer

import java.io.{EOFException, InputStream}
import scala.annotation.tailrec
class Tokenizer(charStream: InputStream){
  var hasNext = true
  var line = 0

  final val IntPattern = "([0-9]+)".r
  final val Delim = "(\\s)".r

  @tailrec
  final def Next(): JgToken = {
    val char = charStream.read()
    char match {
      case -1 => hasNext = false; EOF
      case '+' => ADD
      case '-' => SUB
      case '=' => EQ
      case '*' => MUL
      case '/' => DIV
      case ' ' | '\t' | '\f' => Next()
      case '\n' | '\r' => line += 1; Next()
      case _ => {
        val sb = new StringBuilder()
        sb += char.toChar
        var needNext = true
        while (needNext) {
          val nextChar = charStream.read()
          if (nextChar == -1) {
            needNext = false
          } else {
            nextChar.toChar.toString match {
              case Delim(_) => needNext = false
              case _ => sb += nextChar.toChar
            }
          }
        }
        sb.toString() match {
          case IntPattern(s) => INT(s.toInt)
          case default => throw new IllegalStateException(s"unrecognized identifier, id=$default")
        }
      }
    }
  }

  def HasNext(): Boolean = {
    hasNext
  }
}