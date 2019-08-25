package org.nnc.funexpr

package object ast {

  sealed trait Expr

  final case class ExprIdent(name: String) extends Expr

  final case class ExprValue(value: Double) extends Expr

  final case class ExprFunction(name: String, args: Seq[Expr]) extends Expr

}
