package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.{Expr, ExprFunction, ExprIdent, ExprValue}

class ExprInterpreter(symbolTable: SymbolTable) {
  def exec(expr: Expr): Value = expr match {

    case ExprIdent(name) => symbolTable.getValue(name) match {

      case Some(value) => value

      case _ => throw new Exception("ident not found")
    }

    case ExprValue(value) => DoubleValue(value)

    case ExprFunction(name, args) => symbolTable.getValue(name) match {

      case None => throw new Exception("ident not found")

      // TODO: add args check
      case Some(FunctionValue(_, fun)) => fun(args.map(exec))

      case _ => throw new Exception("not function")
    }
  }
}

