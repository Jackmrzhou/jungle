package c.y.z
package parser

import c.y.z.tokenizer.{DIV, INT, JgToken, MUL, ADD, SUB}

import scala.util.parsing.combinator.Parsers

object JgParser extends Parsers{
  override type Elem = JgToken

  def number: Parser[tokenizer.INT] = accept("int value", { case int @ INT(value) => int})

  def term: Parser[tokenizer.INT] = {
    number ~ rep(MUL ~ term | DIV ~ term) ^^ {
      case left ~ right => right.foldLeft(left) {
        case (x, (MUL ~ y)) => INT(x.value * y.value)
        case (x, (DIV ~ y)) => INT(x.value / y.value)
      }
    }
  }

  def expression: Parser[tokenizer.INT] = {
    term ~ rep(ADD ~ expression | SUB ~ expression) ^^ {
      case left ~ right => right.foldLeft(left) {
        case (x, ADD ~ y) => INT(x.value + y.value)
        case (x, SUB ~ y) => INT(x.value - y.value)
      }
    }
  }

  def apply(tokens: Seq[JgToken]) = {
    val reader = new TokenReader(tokens)
    expression(reader) match {
      case Success(result, next) => Right(result)
    }
  }
}
