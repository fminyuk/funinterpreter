package org.nnc.funexpr

import org.nnc.funexpr.ast.Expr

import scala.reflect.runtime.universe.{Type, TypeTag}

package object interpreters {

  sealed trait Value {
    val tag: Type
  }

  final case class ValueItem[T: TypeTag](value: T) extends Value {
    override val tag: Type = implicitly[TypeTag[T]].tpe
  }

  final case class ValueFunction[T: TypeTag](value: Seq[Value] => Value) extends Value {
    override val tag: Type = implicitly[TypeTag[T]].tpe

    val res: Type = tag.typeArgs.last

    var args: Seq[Type] = tag.typeArgs.init
  }

  sealed trait Error

  final case class ErrorIdentNotFound(name: String) extends Error

  final case class ErrorFunctionNotFound(name: String, args: Seq[Expr]) extends Error

  final case class ErrorNoSuitableFunction(name: String, args: Seq[Expr]) extends Error

  final case class ErrorExprAmbiguous(expr: Expr, required: Type) extends Error

  final case class ErrorExprWrongType(expr: Expr, required: Type) extends Error

}
