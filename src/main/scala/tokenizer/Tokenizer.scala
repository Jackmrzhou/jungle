package c.y.z
package tokenizer

import java.io.{BufferedReader, EOFException, InputStream}
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
class Tokenizer(charStream: BufferedReader){
  var line = 0
  var curLineContent = ""
  var col = 0
  val buffer = ListBuffer[Int]()
  var next = {
    curLineContent = charStream.readLine() + "\n"
    buffer ++= curLineContent.map(_.toInt)
    buffer.head
  }

  final val Digits = "([0-9])".r
  final val IntPattern = "([0-9]+)".r
  final val FloatPattern = "([0-9]*.[0-9]+)".r
  final val Delim = "(\\s)".r

  def ErrorMsg(msg :String):String = {
    s"Error at line:$line, pos:$col, msg:$msg"
  }

  def nextLine(): Unit = {
    line += 1
    col = 0
    charStream.readLine() match {
      case null => buffer += -1
      case default => {
        curLineContent = default + "\n"
        buffer ++= curLineContent.map(_.toInt)
      }
    }
  }

  final def getNextChar: Char = {
    col += 1
    val cur = next.toChar
    next = if (buffer.nonEmpty) {
      val head = buffer.head
      buffer.remove(0)
      head
    } else {
      throw new RuntimeException("EOF")
    }
    cur
  }

  final def putBack(ch: Char) = {
    buffer.insert(0, next)
    buffer.insert(0, ch)
    getNextChar
  }

  def produce(token: JgToken): token.type = token.setPos(Pos(line, col, curLineContent))

  @tailrec
  final def Next(): JgToken = {
    val cur = getNextChar
    cur match {
      case '+' => produce(ADD)
      case '-' => produce(SUB)
      case '=' => produce(EQ)
      case '*' => produce(MUL)
      case '/' => produce(DIV)
      case ' ' | '\t' | '\f' => if (!HasNext()) EOF else Next()
      case '\n' | '\r' => nextLine(); if (!HasNext()) EOF else Next()
      case '(' => produce(LPAREN)
      case ')' => produce(RPAREN)
      case ',' => produce(COMMA)
      case '[' => produce(LBRACKET)
      case ']' => produce(RBRACKET)
      case '{' => produce(LBRACE)
      case '}' => produce(RBRACE)
      case ':' => {
        if(HasNext()) {
          getNextChar match {
            case '=' => produce(COLONEQ)
            case ch => putBack(ch); produce(COLON)
          }
        } else produce(COLON)
      }
      // number literal
      case ch if ch.isDigit => {
        val sb = new StringBuilder()
        sb += cur
        var needNext = true
        while (needNext && HasNext()) {
          val nextChar = getNextChar
          nextChar.toString match {
            case "." | Digits(_) => sb += nextChar
            case _ => needNext = false; putBack(nextChar)
          }
        }
        sb.toString() match {
          case IntPattern(s) => produce(INT(s.toInt))
          case FloatPattern(s) => produce(FLOAT(s.toFloat))
          case default => throw new IllegalStateException(ErrorMsg(s"unrecognized literal, id=$default"))
        }
      }
      // identifier
      case ch if ch.isLetter || ch == '_' => {
        val sb = new StringBuilder()
        sb += cur
        var needNext = true
        while (needNext && HasNext()) {
          val nextChar = getNextChar
          if (nextChar.isLetter || nextChar == '_' || nextChar.isDigit) {
            sb += nextChar
          } else {
            needNext = false
            putBack(nextChar)
          }
        }
        sb.toString() match {
          case "var" => produce(VAR)
          case "int32" => produce(INT32TYPE)
          case "float32" => produce(FLOAT32TYPE)
          case id => produce(ID(id))
        }
      }
    }
  }

  def HasNext(): Boolean = {
    next != -1
  }
}