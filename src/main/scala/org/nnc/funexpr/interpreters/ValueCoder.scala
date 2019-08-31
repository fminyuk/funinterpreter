package org.nnc.funexpr.interpreters

trait ValueCoder[A] {
  def encode(value: A): Value

  def decode(value: Value): A
}
