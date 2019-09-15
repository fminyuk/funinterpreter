package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.{Type, TypeTag}
import org.nnc.funexpr.ast._
import cats.syntax.either._

import scala.annotation.tailrec

class ExprCompiler(symbols: SymbolTable) {
  def compile(expr: Expr): Either[Error, Seq[ExprProgram]] = expr match {
    case ExprFloat(value) => getValueProgram(value)

    case ExprBool(value) => getValueProgram(value)

    case ExprInt(value) => getValueProgram(value)

    case ExprIdent(name) => getIdentProgram(name)

    case ExprFunction(name, args) => getFunctionProgram(name, args)
  }

  private def getValueProgram[T: TypeTag](value: T): Either[Error, Seq[ExprProgram]] = {
    Seq(new ExprCompiler.ExprProgramValue(ValueItem(value))).asRight
  }

  private def getIdentProgram(name: String): Either[Error, Seq[ExprProgram]] = {
    symbols.getValue(name) match {
      case Seq() => ErrorIdentNotFound(name).asLeft
      case values => values.map(v => new ExprCompiler.ExprProgramValue(v)).asRight
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
          args <- ExprCompiler.getCombinations(vars, Seq(Seq()))
          if fun.args == args.map(_.res)
        } yield new ExprCompiler.ExprProgramFunction(fun, args)
      case _ => Seq()
    }
  }
}

object ExprCompiler {

  private class ExprProgramFunction(fun: ValueFunction[_], args: Seq[ExprProgram]) extends ExprProgram {
    override val res: Type = fun.res

    override def exec: Value = fun.value(args.map(_.exec))
  }

  private class ExprProgramValue(value: Value) extends ExprProgram {
    override val res: Type = value.tag

    override def exec: Value = value
  }

  @tailrec
  private def getCombinations[T](vars: Seq[Seq[T]], rest: Seq[Seq[T]]): Seq[Seq[T]] = {
    vars match {
      case head :+ tail =>
        getCombinations(head, for {
          at <- tail
          ar <- rest
        } yield at +: ar)
      case _ => rest
    }
  }
}
