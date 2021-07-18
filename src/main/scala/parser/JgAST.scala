package c.y.z
package parser

import tokenizer._

sealed trait JgAST

sealed trait Expression extends JgAST {
  // name based type system
  var ExprType: Option[Type] = None
}
case class BinaryExpr(left: Expression, Op: JgToken, right: Expression) extends Expression
case class ParenExpr(expr: Expression) extends Expression
case class Literal(token: LITERAL, litType: Type) extends Expression {
  ExprType = Option(litType)
}
case class Identifier(token: ID) extends Expression

case class ExprList(exprs: List[Expression]) extends JgAST

sealed trait Statement extends JgAST
sealed trait SimpleStatement extends Statement
case class ExprStatement(expr: Expression) extends SimpleStatement
case class AssignStatement(l: ExprList, r: ExprList) extends SimpleStatement
case class DeclStatement(ids: List[Identifier], typeName: Option[Type], initVal: Option[ExprList]) extends Statement
case class ShortDeclStmt(ids: List[Identifier], initVal: ExprList) extends SimpleStatement
case class StatementList(stmts: List[Statement]) extends JgAST
case class BlockStmt(stmtList: StatementList) extends Statement
case class IfStmt(cond: Expression, trueBlock: BlockStmt, elseBlock: Option[BlockStmt]) extends Statement

case class TrivialForClause(initStmt: Option[SimpleStatement], cond: Option[Expression], postStmt: Option[SimpleStatement])
case class RangeForClause(assign: AssignStatement, isDecl: Boolean)
case class ForStmt(forClause: Either[TrivialForClause, RangeForClause], blockStmt: BlockStmt) extends Statement

sealed trait Type extends JgAST {
  def ==(that: Type): Boolean
}
case class SliceType(elementType: Type) extends Type {
  override def ==(that: Type): Boolean = that match {
    case SliceType(e) => elementType == e
    case _ => false
  }
}
case class TypeName(token: Identifier) extends Type {
  override def ==(that: Type): Boolean = that match {
    case TypeName(id) => token.token.value == id.token.value
    case _ => false
  }
}

sealed trait BuiltinType extends Type

// builtin types
case object int32 extends BuiltinType {
  override def ==(that: Type): Boolean = that match {
    case _: int32.type => true
    case _ => false
  }
}

case object float32 extends BuiltinType {
  override def ==(that: Type): Boolean = that match {
    case _: float32.type => true
    case _ => false
  }
}

case object string extends BuiltinType {
  override def ==(that: Type): Boolean = that match {
    case _: string.type => true
    case _ => false
  }
}

case object bool extends BuiltinType {
  override def ==(that: Type): Boolean = that match {
    case _: bool.type => true
    case _ => false
  }
}

case class Program(globalStmts: Option[StatementList], blockStmt: Option[BlockStmt]) extends JgAST