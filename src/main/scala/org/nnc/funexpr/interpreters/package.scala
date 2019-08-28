package org.nnc.funexpr

package object interpreters {

  sealed trait Type

  final case class DoubleType() extends Type

  final case class BoolType() extends Type

  final case class FunctionType(result: Type, args: Seq[Type]) extends Type

  sealed trait Value {
    val valueType: Type
  }

  final case class DoubleValue(value: Double) extends Value {
    override val valueType: Type = DoubleType()
  }

  final case class BoolValue(value: Boolean) extends Value {
    override val valueType: Type = BoolType()
  }

  final case class FunctionValue(valueType: Type, value: Seq[Value] => Value) extends Value

}
