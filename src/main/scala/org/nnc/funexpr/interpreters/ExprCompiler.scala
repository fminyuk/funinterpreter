package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.Type
import org.nnc.funexpr.ast._

trait ExprCompiler {
  def compile(expr: Expr, resultType: Type): Either[Error, ExprProgram]
}
