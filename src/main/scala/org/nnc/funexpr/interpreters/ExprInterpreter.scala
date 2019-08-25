package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.{Expr, ExprFunction, ExprIdent, ExprValue}
import org.nnc.funexpr.interpreters.values._

class ExprInterpreter(symbolTable: SymbolTable) {
  def exec(expr: Expr): Value = expr match {

    case ExprIdent(name) => symbolTable.getValue(name)

    case ExprValue(value) => DoubleValue(value)

    case ExprFunction(name, args) => symbolTable.getValue(name) match {

      // TODO: add args check
      case FunctionValue(fun) => fun(args.map(exec))

      case _ => throw new Exception("not function")

    }

  }
}

