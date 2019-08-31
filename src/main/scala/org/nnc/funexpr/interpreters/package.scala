package org.nnc.funexpr

package object interpreters {

  sealed trait Value

  final case class ValueItem[T](value: T) extends Value

  final case class ValueFunction(value: Seq[Value] => Value) extends Value
}
