package c.y.z
package type_checker

import parser._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Scope(childScopes: ListBuffer[Scope], parentScope: Scope) {
  val identifiers: mutable.Map[Identifier, Type] = mutable.Map[Identifier, Type]()
  // init with builtin types
  val types: mutable.Set[Type] = mutable.Set[Type]()

  def isBasicType(t: Type): Boolean = {
    t match {
      case _: BuiltinType => true
      case SliceType(e) => isBasicType(e)
      case _ => false
    }
  }

  def GetIdType(id: Identifier): Option[Type] = identifiers.get(id)

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
          // first add ids and into scopt
          val declErr = if (types.contains(t) || isBasicType(t)) {
            declStmt.ids.foldLeft(Option.empty[String]) {
              case (opt, id) => {
                if (opt.isEmpty) {
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
