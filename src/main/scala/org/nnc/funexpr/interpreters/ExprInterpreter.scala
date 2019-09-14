package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.TypeTag
import org.nnc.funexpr.ast._
import cats.syntax.either._

class ExprInterpreter(compiler: ExprCompiler) {
  def exec[R: TypeTag: ValueCoder](expr: Expr): Either[Error, R] = {
    for {
      programs <- compiler.compile(expr)
      value <- exec(programs)
    } yield implicitly[ValueCoder[R]].decode(value)
  }

  private def exec[R: TypeTag](programs: Seq[ExprProgram]): Either[Error, Value] = {
    val requiredType = implicitly[TypeTag[R]].tpe

    programs.filter(_.res == requiredType) match {
      case Seq() => ErrorExprWrongType(requiredType).asLeft
      case Seq(one) => one.exec.asRight
      case _ => ErrorExprAmbiguous(requiredType).asLeft
    }
  }
}

