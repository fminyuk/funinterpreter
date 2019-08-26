package org.nnc.funexpr.interpreters.values

import org.nnc.funexpr.interpreters.types.Type

trait ValuesConverter[A] {
  val valueType: Type

  def encode(value: A): Value

  def decode(value: Value): A
}
