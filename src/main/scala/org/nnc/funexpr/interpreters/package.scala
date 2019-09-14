package org.nnc.funexpr

import scala.reflect.runtime.universe._

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

  final case class ErrorIdentNotFound(ident: String) extends Error

  final case class ErrorFunctionNotFound(ident: String) extends Error

  final case class ErrorExprWrongType(required: Type) extends Error

  final case class ErrorExprAmbiguous(required: Type) extends Error

}
