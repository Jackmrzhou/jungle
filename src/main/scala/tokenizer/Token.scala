package c.y.z
package tokenizer

import scala.util.parsing.input.{Position, Positional}

sealed trait JgToken extends Positional

case class Pos(_line: Int, col: Int, lineStr: String) extends Position {
  override def line: Int = _line
  override def column: Int = col

  override protected def lineContents: String = lineStr
}

case object ADD extends JgToken
case object SUB extends JgToken
case object EQ extends JgToken
case object MUL extends JgToken
case object DIV extends JgToken
case object EOF extends JgToken
case object LPAREN extends JgToken
case object RPAREN extends JgToken
case object LBRACKET extends JgToken
case object RBRACKET extends JgToken
case object LBRACE extends JgToken
case object RBRACE extends JgToken
case object COMMA extends JgToken
case object COLONEQ extends JgToken
case object COLON extends JgToken

sealed trait LITERAL extends JgToken

sealed trait NUMERIC extends LITERAL {
  def +(y: NUMERIC): NUMERIC
  def -(y: NUMERIC): NUMERIC
  def /(y: NUMERIC): NUMERIC
  def *(y: NUMERIC): NUMERIC
}
case class INT(value: Int) extends NUMERIC {
  def +(other: NUMERIC): NUMERIC = other match {
    case INT(value) => INT(this.value + value)
    case FLOAT(value) => FLOAT(this.value + value)
  }
  def -(y: NUMERIC): NUMERIC = y match {
    case INT(value) => INT(this.value - value)
    case FLOAT(value) => FLOAT(this.value - value)
  }
  def /(y: NUMERIC): NUMERIC = y match {
    case INT(value) => INT(this.value / value)
    case FLOAT(value) => FLOAT(this.value / value)
  }

  def *(y: NUMERIC): NUMERIC = y match {
    case INT(value) => INT(this.value * value)
    case FLOAT(value) => FLOAT(this.value * value)
  }
}
case class FLOAT(value: Float) extends NUMERIC {
  override def +(y: NUMERIC): NUMERIC = y match {
    case i: INT => i + this
    case FLOAT(value) => FLOAT(this.value + value)
  }

  override def -(y: NUMERIC): NUMERIC = y match {
    case INT(value) => FLOAT(this.value - value)
    case FLOAT(value) => FLOAT(this.value - value)
  }

  override def /(y: NUMERIC): NUMERIC = y match {
    case INT(value) => FLOAT(this.value / value)
    case FLOAT(value) => FLOAT(this.value / value)
  }

  override def *(y: NUMERIC): NUMERIC = y match {
    case i: INT => i * this
    case FLOAT(value) => FLOAT(this.value * value)
  }
}

case class ID(value: String) extends JgToken

sealed trait KEYWORD extends JgToken
case object VAR extends KEYWORD
case object INT32TYPE extends KEYWORD
case object FLOAT32TYPE extends KEYWORD