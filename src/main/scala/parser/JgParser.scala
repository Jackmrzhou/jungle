package c.y.z
package parser

import c.y.z.tokenizer._
import c.y.z.utils.Logger

import scala.util.parsing.combinator.{PackratParsers, Parsers}

object JgParser extends Parsers {
  override type Elem = JgToken

  def literal: Parser[Literal] = accept("literal", {
    case int @ INT(_) => Literal(int, int32)
    case float @ FLOAT(_) => Literal(float, float32)
    case s @ STRING(_) => Literal(s, string)
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

  def arithExpr: Parser[Expression] = term ~ rep((ADD | SUB) ~ term) ^^ {
    case l ~ r => r.foldLeft(l) {
        case (l, op ~ r) => BinaryExpr(l, op, r)
      }
  }

  def binaryExpr: Parser[Expression] = arithExpr ~ rep((EQEQ | NOTEQ) ~ arithExpr) ^^ {
    case l ~ r => r.foldLeft(l) {
      case (l, op ~ r) => BinaryExpr(l, op, r)
    }
  }

  def exprList: Parser[ExprList] = repsep(expression, COMMA) ^^ (l => ExprList(l))

  def primaryExpr: Parser[Expression] = operand

  def expression: Parser[Expression] =  binaryExpr ^^ {
    e => Logger.debug(s"Match expression: ${e}"); e
  }

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

  def simpleStatement: Parser[SimpleStatement] = shortDeclStatement | assignStatement | exprStatement ^^{
    s => Logger.debug(s"Match simpleStatement: ${s}"); s
  }

  def statement: Parser[Statement] = forStmt | ifStmt | fullDeclStatement | simpleStatement

  def statementList: Parser[StatementList] = rep1(statement) ^^ {
    s => StatementList(s.foldLeft(List[Statement]()) {
        case (l, s) => l :+ s
      })
  }

  def blockStmt: Parser[BlockStmt] = LBRACE ~> opt(statementList) <~ RBRACE ^^ {
    stmtList => Logger.debug(s"Match blockStmt: $stmtList"); BlockStmt(stmtList.getOrElse(StatementList(Nil)))
  }

  def ifStmt: Parser[IfStmt] = IF ~> expression ~ blockStmt ~ opt(ELSE ~> blockStmt) ^^ {
    case expr ~ trueBlock ~ elseBlock => IfStmt(expr, trueBlock, elseBlock)
  }

  def rangeForClause: Parser[RangeForClause] = FOR ~> exprList ~ (EQ | COLONEQ) ~ RANGE ~ expression ^^ {
    case exprs ~ op ~ _ ~ expr => RangeForClause(AssignStatement(exprs, ExprList(List(expr))), op == COLONEQ)
  }

  def trivialForClause1: Parser[TrivialForClause] =
    FOR ~> (opt(simpleStatement) ~ SEMICOLON ~ opt(expression) ~ SEMICOLON ~ opt(simpleStatement)) ^^ {
      case initStmt ~ _ ~ expr ~ _ ~ postStmt => Logger.debug("Match trivialForClause1"); TrivialForClause(initStmt, expr, postStmt)
  }

  def trivialForClause2: Parser[TrivialForClause] = FOR ~> opt(expression) ^^ {
    expr => TrivialForClause(None, expr, None)
  }

  def forStmt: Parser[Statement] = (rangeForClause | trivialForClause1 | trivialForClause2) ~ blockStmt ^^ {
    case clause ~ blockStmt => Logger.debug(s"Match ForStmt: $clause, $blockStmt") ;clause match {
      case r: RangeForClause=> ForStmt(Right(r), blockStmt)
      case l: TrivialForClause=> ForStmt(Left(l), blockStmt)
    }
  }

  def program: Parser[Program] = opt(statementList) ~ opt(blockStmt) ^^ {
    case stmtList ~ blockStmt => Program(stmtList, blockStmt)
  }

  def apply(tokens: Seq[JgToken]) = {
    val reader = new TokenReader(tokens)
    phrase(program)(reader) match {
      case Success(result, next) => Right(result)
      case Error(msg, next) => Left(msg)
    }
  }
}
