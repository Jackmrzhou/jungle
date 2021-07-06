package c.y.z
package parser

import tokenizer._

sealed trait JgAST

sealed trait Expression extends JgAST
case class BinaryExpr(left: Expression, Op: JgToken, right: Expression) extends Expression
case class ParenExpr(expr: Expression) extends Expression
case class Literal(token: LITERAL) extends Expression
case class Identifier(token: ID) extends Expression

case class ExprList(exprs: List[Expression]) extends JgAST

sealed trait Statement extends JgAST
case class ExprStatement(expr: Expression) extends Statement
case class AssignStatement(l: ExprList, r: ExprList) extends Statement
case class DeclStatement(ids: List[Identifier], typeName: Option[TypeName], initVal: Option[ExprList]) extends Statement
case class ShortDeclStmt(ids: List[Identifier], initVal: ExprList) extends Statement
case class StatementList(stmts: List[Statement]) extends JgAST

case class TypeName(token: Identifier) extends JgAST