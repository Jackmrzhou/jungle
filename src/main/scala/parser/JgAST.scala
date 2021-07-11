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
case class ExprStatement(expr: Expression) extends Statement
case class AssignStatement(l: ExprList, r: ExprList) extends Statement
case class DeclStatement(ids: List[Identifier], typeName: Option[Type], initVal: Option[ExprList]) extends Statement
case class ShortDeclStmt(ids: List[Identifier], initVal: ExprList) extends Statement
case class StatementList(stmts: List[Statement]) extends JgAST

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
    case int32 => true
    case _ => false
  }
}

case object float32 extends BuiltinType {
  override def ==(that: Type): Boolean = that match {
    case float32 => true
    case _ => false
  }
}
