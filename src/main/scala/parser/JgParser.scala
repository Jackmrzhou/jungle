package c.y.z
package parser

import c.y.z.tokenizer._

import scala.util.parsing.combinator.{PackratParsers, Parsers}

object JgParser extends Parsers {
  override type Elem = JgToken

  def literal: Parser[Literal] = accept("literal", {
    case int @ INT(_) => Literal(int, int32)
    case float @ FLOAT(_) => Literal(float, float32)
  })

  def identifier: Parser[Identifier] = accept("identifier", {
    case id @ ID(_) => Identifier(id)
  })

  def idList: Parser[List[Identifier]] = repsep(identifier, COMMA)

  def operand: Parser[Expression] = literal | identifier | LPAREN ~> expression <~ RPAREN

  def term: Parser[Expression] = primaryExpr ~ rep((MUL | DIV) ~ primaryExpr)^^ {
    case l ~ r => r.foldLeft(l) {
      case (l, op ~ r) => BinaryExpr(l, op, r)
    }
  }

  def binaryExpr: Parser[Expression] = term ~ rep1((ADD | SUB) ~ term) ^^ {
    case l ~ r => r.foldLeft(l) {
      case (l, op ~ r) => BinaryExpr(l, op, r)
    }
  }

  def exprList: Parser[ExprList] = repsep(expression, COMMA) ^^ (l => ExprList(l))

  def primaryExpr: Parser[Expression] = operand

  def expression: Parser[Expression] =  binaryExpr | term | primaryExpr

  def exprStatement: Parser[ExprStatement] = expression ^^ (expr => ExprStatement(expr))

  def assignStatement: Parser[AssignStatement] = exprList ~ EQ ~ exprList ^^ {
    case l ~ _ ~ r => AssignStatement(l, r)
  }

  def jgType: Parser[Type] = typeName | sliceType

  def typeLit: Parser[Type] = sliceType

  def sliceType: Parser[Type] = LBRACKET ~ RBRACKET ~> jgType ^^ {t => SliceType(t)}

  def builtinTypes: Parser[Type] = (INT32TYPE | FLOAT32TYPE) ^^ {
    case INT32TYPE => int32
    case FLOAT32TYPE => float32
  }

  def typeName: Parser[Type] = (builtinTypes | identifier) ^^ {
    case id: Identifier => TypeName(id)
    case t: Type => t
  }

  def fullDeclStatement: Parser[DeclStatement] = VAR ~ idList ~ opt(jgType) ~ opt(EQ ~ exprList) ^^ {
    case _ ~ ids ~ t ~ init => DeclStatement(ids, t, init.map(_._2))
  }

  def shortDeclStatement: Parser[ShortDeclStmt] = idList ~ COLONEQ ~ exprList ^^ {
    case ids ~ _ ~ exprs => ShortDeclStmt(ids, exprs)
  }

  def declStatement: Parser[Statement] = shortDeclStatement | fullDeclStatement

  def statement: Parser[Statement] = declStatement | assignStatement | exprStatement

  def statementList: Parser[JgAST] = rep1(statement) ^^ {
    s => StatementList(s.foldLeft(List[Statement]()) {
        case (l, s) => l :+ s
      })
  }

  def program: Parser[JgAST] = phrase(statementList)

  def apply(tokens: Seq[JgToken]) = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case Success(result, next) => Right(result)
      case Error(msg, next) => Left(msg)
    }
  }
}
