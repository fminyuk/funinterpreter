package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.TypeTag
import org.nnc.funexpr.ast._

trait ExprInterpreter {
  def exec[R: TypeTag: ValueCoder](expr: Expr): Either[Error, R]
}

