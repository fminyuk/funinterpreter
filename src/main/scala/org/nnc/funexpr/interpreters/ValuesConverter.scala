package org.nnc.funexpr.interpreters

trait ValuesConverter[A] {
  val valueType: Type

  def encode(value: A): Value

  def decode(value: Value): A
}
