package c.y.z
package type_checker

import parser._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Scope(childScopes: ListBuffer[Scope], parentScope: Option[Scope]) {
  val identifiers: mutable.Map[Identifier, Type] = mutable.Map[Identifier, Type]()

  def addChild(scope: Scope): Unit = {
    childScopes += scope
  }

  def SpawnNewScope(): Scope = {
    val newScope = new Scope(ListBuffer[Scope](), Option(this))
    addChild(newScope)
    newScope
  }

  def isBasicType(t: Type): Boolean = {
    t match {
      case _: BuiltinType => true
      case SliceType(e) => isBasicType(e)
      case _ => false
    }
  }

  // will get current scope's type if some
  // or get from parents scopes if not found in current scope
  def GetIdType(id: Identifier): Option[Type] = {
    val local = identifiers.get(id)
    if (local.nonEmpty) local
    else parentScope match {
      case None => None
      case Some(value) => value.GetIdType(id)
    }
  }

  def NewShortDecl(decl: ShortDeclStmt): Option[String] = {
    if (decl.ids.length != decl.initVal.exprs.length) {
      Option(s"initial values should be equal to variable, Pos: ${decl.ids.head.token.pos}")
    } else {
      decl.ids.zip(decl.initVal.exprs).foldLeft(Option.empty[String]) {
        case (err: Some[String], _) => err
        case (_, (id, expr)) => {
          if (expr.ExprType.isEmpty) throw new IllegalStateException(s"expression should have type populated, expr=${expr}")
          val exprType = expr.ExprType.get
          identifiers.get(id) match {
            case None => identifiers.put(id, exprType); errors.NilError
            case Some(t) => {
              if (!(t == exprType)) Option(s"type not equal, left id's type=${t}, right expression type=${exprType}")
              else errors.NilError
            }
          }
        }
      }
    }
  }

  def NewDeclaration(declStmt :DeclStatement): Option[String] = {
    (declStmt.typeName, declStmt.initVal) match {
        case (None, None) => Option(s"declaration cannot must have type or initialization list, ids pos=${declStmt.ids.head.token.pos}")
        case (Some(t), _) => {
          // first add ids and into scope
          val declErr = if (isBasicType(t)) {
            declStmt.ids.foldLeft(Option.empty[String]) {
              case (opt, id) => {
                if (opt.isEmpty) {
                  // we allow redeclaration in nested scopes
                  // but can not in current scope
                  if (identifiers.contains(id)) {
                    Option(s"redeclare variable '${id.token.value}' at \n${id.token.pos.longString}")
                  } else {
                    identifiers.put(id, t)
                    opt
                  }
                } else {
                  opt
                }
              }
            }
          } else {
            Option(s"type not found, ${t.toString}")
          }
          declErr match {
            case Some(_) => declErr
            case None => if (declStmt.initVal.nonEmpty) NewShortDecl(ShortDeclStmt(declStmt.ids, declStmt.initVal.get)) else Option.empty[String]
          }
        }
        case (None, Some(initVal)) => NewShortDecl(ShortDeclStmt(declStmt.ids, initVal))
    }
  }
}
