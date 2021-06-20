package c.y.z
package parser

import c.y.z.tokenizer.JgToken

import scala.util.parsing.input.{NoPosition, Position, Reader}

class TokenReader(tokens: Seq[JgToken]) extends Reader[JgToken]{
  override def first: JgToken = tokens.head

  override def rest: Reader[JgToken] = new TokenReader(tokens.tail)

  override def pos: Position = NoPosition

  override def atEnd: Boolean = tokens.isEmpty
}
