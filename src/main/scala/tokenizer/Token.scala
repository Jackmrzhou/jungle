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
case object EQEQ extends JgToken
case object EXCL extends JgToken
case object NOTEQ extends JgToken
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
case object SEMICOLON extends JgToken

sealed trait LITERAL extends JgToken
case class INT(value: Int) extends LITERAL
case class FLOAT(value: Float) extends LITERAL
case class STRING(value: String) extends LITERAL

case class ID(value: String) extends JgToken

sealed trait KEYWORD extends JgToken
case object VAR extends KEYWORD
case object INT32TYPE extends KEYWORD
case object FLOAT32TYPE extends KEYWORD
case object IF extends KEYWORD
case object ELSE extends KEYWORD
case object RANGE extends KEYWORD
case object FOR extends KEYWORD