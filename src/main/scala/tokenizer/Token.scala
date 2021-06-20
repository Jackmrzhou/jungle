package c.y.z
package tokenizer

sealed trait JgToken

case object ADD extends JgToken
case object SUB extends JgToken
case object EQ extends JgToken
case object MUL extends JgToken
case object DIV extends JgToken
case object EOF extends JgToken
case class INT(value: Int) extends JgToken