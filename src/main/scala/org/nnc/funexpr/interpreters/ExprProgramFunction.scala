package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.Type

class ExprProgramFunction(fun: ValueFunction[_], args: Seq[ExprProgram]) extends ExprProgram {
  override val res: Type = fun.res

  override def exec: Value = fun.value(args.map(_.exec))
}
