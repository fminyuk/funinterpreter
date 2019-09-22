package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.TypeTag
import org.nnc.funexpr.ast.Expr

class ExprInterpreterImpl(compiler: ExprCompiler) extends ExprInterpreter {
  override def exec[R: TypeTag: ValueCoder](expr: Expr): Either[Error, R] = {
    val requiredType = implicitly[TypeTag[R]].tpe
    val coder = implicitly[ValueCoder[R]]

    for {
      program <- compiler.compile(expr, requiredType)
    } yield coder.decode(program.exec)
  }
}


