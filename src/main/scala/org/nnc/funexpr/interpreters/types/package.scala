package org.nnc.funexpr.interpreters

package object types {

  sealed trait Type

  final case class DoubleType() extends Type

  final case class BoolType() extends Type

  final case class FunctionType(result: Type, args: Seq[Type]) extends Type

}
