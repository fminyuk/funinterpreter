package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.Type

class ExprProgramValue(value: Value) extends ExprProgram {
  override val res: Type = value.tag

  override def exec: Value = value
}
