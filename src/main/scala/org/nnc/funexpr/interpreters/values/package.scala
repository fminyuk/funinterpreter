package org.nnc.funexpr.interpreters

package object values {

  sealed trait Value

  final case class DoubleValue(value: Double) extends Value

  final case class BoolValue(value: Boolean) extends Value

  final case class FunctionValue(value: Seq[Value] => Value) extends Value

}
