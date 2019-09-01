package org.nnc.funexpr

import scala.reflect.runtime.universe._

package object interpreters {

  sealed trait Value

  final case class ValueItem[T](value: T) extends Value

  final case class ValueFunction(value: Seq[Value] => Value) extends Value

  sealed trait NewValue {
    val tag: Type
  }

  final case class NewValueItem[T: TypeTag](value: T) extends NewValue {
    override val tag: Type = implicitly[TypeTag[T]].tpe
  }

  final case class NewValueFunction[T: TypeTag](value: T) extends NewValue {
    override val tag: Type = implicitly[TypeTag[T]].tpe

    val res: Type = tag.typeArgs.last

    var args: Seq[Type] = tag.typeArgs.init
  }
}
