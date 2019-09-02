package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.{Expr, ExprFunction, ExprIdent, ExprValue}

class ExprInterpreter(symbolTable: SymbolTable) {
  def exec(expr: Expr): Either[Error, Value] = expr match {

    case ExprIdent(name) => symbolTable.getValue(name) match {
      case None => Left(ErrorIdentNotFound(name))
      case Some(value) => Right(value)
    }

    case ExprValue(value) => Right(ValueItem[Double](value))

    case ExprFunction(name, args) => symbolTable.getValue(name) match {
      case None => Left(ErrorIdentNotFound(name))

      case Some(value) => value match {
        case fun: ValueFunction[_] =>
          args.foldLeft(Right(Seq()): Either[Error, Seq[Value]]) { (acc, arg) =>
            for {
              s <- acc.right
              l <- exec(arg)
            } yield s :+ l
          } flatMap { values =>
            val types = values.map(_.tag)
            if (types == fun.args) {
              Right(fun.value(values))
            } else Left(ErrorFunctionArgumentsMismatch(name, fun.args, types))
          }
        case _ => Left(ErrorIdentNotFunction(name))
      }
    }
  }
}

