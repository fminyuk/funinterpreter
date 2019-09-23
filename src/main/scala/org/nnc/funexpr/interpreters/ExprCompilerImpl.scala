package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe.{Type, TypeTag}
import org.nnc.funexpr.ast._
import cats.syntax.either._

class ExprCompilerImpl(symbols: SymbolTable) extends ExprCompiler {
  override def compile(expr: Expr, resultType: Type): Either[Error, ExprProgram] = for {
    programs <- compile(expr)

    program <- selectOrConvert(programs, resultType) match {
      case Seq() => ErrorExprWrongType(expr, resultType).asLeft
      case Seq(p) => p.asRight
      case _ => ErrorExprAmbiguous(expr, resultType).asLeft
    }
  } yield program

  private def compile(expr: Expr): Either[Error, Seq[ExprProgram]] = expr match {
    case ExprFloat(value) => compileValue(value)

    case ExprBool(value) => compileValue(value)

    case ExprInt(value) => compileValue(value)

    case ExprIdent(name) => compileIdent(name)

    case ExprFunction(name, args) => compileFunction(name, args)
  }

  private def selectOrConvert(programs: Seq[ExprProgram], resultType: Type): Seq[ExprProgram] = {
    programs.filter(_.res == resultType) match {
      case Seq() =>
        for {
          p <- programs
          c <- symbols.getValue(SymbolTable.IMPLICIT).foldLeft(Seq(): Seq[ValueFunction[_]]) { (acc, value) =>
            value match {
              case f: ValueFunction[_] if f.res == resultType => f.args match {
                case Seq(src) if src == p.res => f +: acc
                case _ => acc
              }
              case _ => acc
            }
          }
        } yield new ExprCompilerImpl.ExprProgramFunction(c, Seq(p))
      case some => some
    }
  }

  private def compileValue[T: TypeTag](value: T): Either[Error, Seq[ExprProgram]] = {
    Seq(new ExprCompilerImpl.ExprProgramValue(ValueItem(value))).asRight
  }

  private def compileIdent(name: String): Either[Error, Seq[ExprProgram]] = {
    symbols.getValue(name) match {
      case Seq() => ErrorIdentNotFound(name).asLeft
      case values => values.map(new ExprCompilerImpl.ExprProgramValue(_)).asRight
    }
  }

  private def compileFunction(name: String, args: Seq[Expr]): Either[Error, Seq[ExprProgram]] = {
    for {
      functions <- getFunctions(name, args)
      variants <- getVariants(args)

      programs <- {
        functions.foldLeft(Right(Seq()): Either[Error, Seq[ExprProgram]]) {
          (acc, function) =>
            for {
              p <- acc
              r <- {
                val variant = variants.zip(function.args).map {
                  case (p, t) => selectOrConvert(p, t)
                }

                if (variant.exists(_.isEmpty)) {
                  p.asRight
                } else {
                  args.indices.filter(variant(_).size > 1) match {
                    case Seq(h, _@_*) => ErrorExprAmbiguous(args(h), function.args(h)).asLeft
                    case _ => (p :+ new ExprCompilerImpl.ExprProgramFunction(function, variant.map(_.head))).asRight
                  }
                }
              }
            } yield r
        }
      }

      result <- programs.size match {
        case 0 => ErrorNoSuitableFunction(name, args).asLeft
        case _ => programs.asRight
      }
    } yield result
  }

  private def getFunctions(name: String, args: Seq[Expr]): Either[Error, Seq[ValueFunction[_]]] = {
    val functions = symbols.getValue(name).foldLeft(Seq(): Seq[ValueFunction[_]]) {
      (acc, value) =>
        value match {
          case f: ValueFunction[_] if f.args.size == args.size => acc :+ f
          case _ => acc
        }
    }

    functions.size match {
      case 0 => ErrorFunctionNotFound(name, args).asLeft
      case _ => functions.asRight
    }
  }

  private def getVariants(args: Seq[Expr]): Either[Error, Seq[Seq[ExprProgram]]] = {
    args.foldLeft(Right(Seq()): Either[Error, Seq[Seq[ExprProgram]]]) {
      (acc, arg) =>
        for {
          s <- acc
          l <- compile(arg)
        } yield s :+ l
    }
  }
}

object ExprCompilerImpl {

  private class ExprProgramFunction(fun: ValueFunction[_], args: Seq[ExprProgram]) extends ExprProgram {
    override val res: Type = fun.res

    override def exec: Value = fun.value(args.map(_.exec))
  }

  private class ExprProgramValue(value: Value) extends ExprProgram {
    override val res: Type = value.tag

    override def exec: Value = value
  }

}

