package c.y.z
package type_checker

import parser._

import c.y.z.tokenizer._

import scala.collection.mutable.ListBuffer
import scala.util.Success

object Checker {
  def newEmptyBlock = BlockStmt(StatementList(Nil))

  def fillExprType(expr: Expression, scope: Scope): Option[String] = {
    expr match {
      case BinaryExpr(left, op, right) => {
        val lErr = fillExprType(left, scope)
        if (lErr.nonEmpty) return lErr
        val rErr = fillExprType(right, scope)
        if (rErr.nonEmpty) return rErr
        val zipped = left.ExprType zip right.ExprType
        if (zipped.nonEmpty && zipped.head._1 == zipped.head._2) {
          expr.ExprType = op match {
            case EQEQ | NOTEQ => Option(bool)
            case _ => left.ExprType
          }
          errors.NilError
        } else {
          Option(s"type not equal, left=${left.ExprType}, right=${right.ExprType}")
        }
      }
      case ParenExpr(expr) => fillExprType(expr, scope)
      case id: Identifier =>  scope.GetIdType(id) match {
        case None => Option(s"identifier not declared, id=${id.token.value}")
        case default => id.ExprType = default; errors.NilError
      }
      case _: Literal => errors.NilError
    }
  }

  def fillExprListType(exprList: ExprList, scope: Scope): Option[String] = {
    exprList.exprs.foldLeft(errors.NilError){
      case (err: Some[String], _) => err
      case (_, expr) => fillExprType(expr, scope)
    }
  }

  def checkAssignment(assign: AssignStatement, scope: Scope): Option[String] = {
    val leftRes = fillExprListType(assign.l, scope)
    if (leftRes.nonEmpty) leftRes
    else {
      val rightRes = fillExprListType(assign.r, scope)
      if (rightRes.nonEmpty) rightRes
      else {
        // perform type checking
        assign.l.exprs.zip(assign.r.exprs).foldLeft(errors.NilError) {
          case (err: Some[String], _) => err
          case (_, (l, r)) =>
            if (!(l.ExprType == r.ExprType))
              Option(s"expression type not equal, left expr:${l}, right expr:$r")
            else
              errors.NilError
        }
      }
    }
  }

  def check(ast: JgAST, scope: Scope): Option[String] = {
    ast match {
      case Program(globalStmts, blockStmt) => {
        val res = check(globalStmts.getOrElse(StatementList(Nil)), scope)
        if (res.nonEmpty) res
        else {
          val newScope = scope.SpawnNewScope()
          check(blockStmt.getOrElse(newEmptyBlock).stmtList, newScope)
        }
      }
      case StatementList(stmts) => stmts.foldLeft(Option.empty[String]) {
        case (err: Some[String], _) => err
        case (_, ds: DeclStatement) => {
          ds.initVal.getOrElse(ExprList(Nil)).exprs.foldLeft(errors.NilError){
            case (err: Some[String], _) => err
            case (_, expr: Expression) => fillExprType(expr, scope)
          }
          scope.NewDeclaration(ds)
        }
        case (_, sd: ShortDeclStmt) => {
          sd.initVal.exprs.foldLeft(errors.NilError) {
            case (err: Some[String], _) => err
            case (_, expr: Expression) => fillExprType(expr, scope)
          }
          scope.NewShortDecl(sd)
        }
        case (_, assign: AssignStatement) => {
          checkAssignment(assign, scope)
        }
        case (_, ifStmt: IfStmt) => {
          val checkCond = fillExprType(ifStmt.cond, scope)
          if (checkCond.nonEmpty) {
            checkCond
          } else if (!(bool == ifStmt.cond.ExprType.get)) {
            Option(s"condition must be boolean type, got: ${ifStmt.cond.ExprType.get}")
          } else {
            val checkFirstBlock = check(ifStmt.trueBlock, scope.SpawnNewScope())
            if (checkFirstBlock.nonEmpty) checkFirstBlock
            else check(ifStmt.elseBlock.getOrElse(newEmptyBlock).stmtList, scope.SpawnNewScope())
          }
        }
        case _ => errors.NilError
      }
      case _ => Option.empty
    }
  }
}
