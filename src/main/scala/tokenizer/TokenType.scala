package c.y.z
package tokenizer

object TokenType extends Enumeration {
  type TokenType = Value

  val Add, Sub, Eq, Mul, Div, Int, EOF = Value
}
