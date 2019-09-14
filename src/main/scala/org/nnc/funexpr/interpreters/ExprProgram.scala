package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe._

trait ExprProgram {
  val res: Type

  def exec: Value
}
