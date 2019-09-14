package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.TypeTag
import org.nnc.funexpr.ast._
import cats.syntax.either._

class ExprCompiler(symbols: SymbolTable) {
  def compile(expr: Expr): Either[Error, Seq[ExprProgram]] = expr match {
    case ExprFloat(value) => getValueProgram(value)

    case ExprBool(value) => getValueProgram(value)

    case ExprInt(value) => getValueProgram(value)

    case ExprIdent(name) => getIdentProgram(name)

    case ExprFunction(name, args) => getFunctionProgram(name, args)
  }

  private def getValueProgram[T: TypeTag](value: T): Either[Error, Seq[ExprProgram]] = {
    Seq(new ExprProgramValue(ValueItem(value))).asRight
  }

  private def getIdentProgram(name: String): Either[Error, Seq[ExprProgram]] = {
    symbols.getValue(name) match {
      case Seq() => ErrorIdentNotFound(name).asLeft
      case values => values.map(v => new ExprProgramValue(v)).asRight
    }
  }

  private def getFunctionProgram(name: String, args: Seq[Expr]): Either[Error, Seq[ExprProgram]] = {
    for {
      vars <- compileArgs(args)
      fun <- {
        {
          for {
            value <- symbols.getValue(name)
            fun <- buildFunctions(value, vars)
          } yield fun
        } match {
          case Seq() => ErrorFunctionNotFound(name).asLeft
          case functions => functions.asRight
        }
      }
    } yield fun
  }

  private def compileArgs(args: Seq[Expr]): Either[Error, Seq[Seq[ExprProgram]]] = {
    args.foldLeft(Right(Seq()): Either[Error, Seq[Seq[ExprProgram]]]) { (acc, arg) =>
      for {
        s <- acc
        l <- compile(arg)
      } yield s :+ l
    }
  }

  private def buildFunctions(value: Value, vars: Seq[Seq[ExprProgram]]): Seq[ExprProgram] = {
    value match {
      case fun: ValueFunction[_] =>
        for {
          args <- getCombinations(vars)
          if fun.args == args.map(_.res)
        } yield new ExprProgramFunction(fun, args)
      case _ => Seq()
    }
  }

  private def getCombinations(vars: Seq[Seq[ExprProgram]]): Seq[Seq[ExprProgram]] = {
    vars match {
      case Seq() => Seq(Seq())
      case Seq(h) => h.map(Seq(_))
      case Seq(h, tail @ _ *) =>
        for {
          ah <- h
          at <- getCombinations(tail)
        } yield ah +: at
    }
  }
}
