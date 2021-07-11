package c.y.z
package type_checker

import parser.{BinaryExpr, DeclStatement, ExprList, Expression, Identifier, JgAST, Literal, ParenExpr, ShortDeclStmt, StatementList}

import scala.collection.mutable.ListBuffer
import scala.util.Success

object Checker {

  def fillAndCheckType(expr: Expression, scope: Scope): Option[String] = {
    expr match {
      case BinaryExpr(left, op, right) => {
        val lErr = fillAndCheckType(left, scope)
        if (lErr.nonEmpty) return lErr
        val rErr = fillAndCheckType(right, scope)
        if (rErr.nonEmpty) return rErr
        val zipped = left.ExprType zip right.ExprType
        if (zipped.nonEmpty && zipped.head._1 == zipped.head._2) {
          expr.ExprType = Option(left.ExprType.get)
          errors.NilError
        } else {
          Option(s"type not equal, left=${left.ExprType}, right=${right.ExprType}")
        }
      }
      case ParenExpr(expr) => fillAndCheckType(expr, scope)
      case id: Identifier =>  scope.GetIdType(id) match {
        case None => Option(s"identifier not declared, id=${id.token.value}")
        case default => id.ExprType = default; errors.NilError
      }
      case lit: Literal => errors.NilError
    }
  }

  def check(ast: JgAST, scope: Scope): Option[String] = {
    ast match {
      case StatementList(stmts) => stmts.foldLeft(Option.empty[String]) {
        case (err: Some[String], _) => err
        case (_, ds: DeclStatement) => {
          ds.initVal.getOrElse(ExprList(Nil)).exprs.foldLeft(errors.NilError){
            case (err: Some[String], _) => err
            case (_, expr: Expression) => fillAndCheckType(expr, scope)
          }
          scope.NewDeclaration(ds)
        }
        case (_, sd: ShortDeclStmt) => {
          sd.initVal.exprs.foldLeft(errors.NilError) {
            case (err: Some[String], _) => err
            case (_, expr: Expression) => fillAndCheckType(expr, scope)
          }
          scope.NewShortDecl(sd)
        }
      }
      case _ => Option.empty
    }
  }
}
