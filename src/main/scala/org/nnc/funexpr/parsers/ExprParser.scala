package org.nnc.funexpr.parsers

import org.nnc.funexpr.ast._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

trait ExprParser extends RegexParsers with PackratParsers {

  protected val float: Regex = """[+-]?\d*((\.\d+([eE][+-]?[0-9]+)?[fF]?)|([fF])|([eE][+-]?[0-9]+))""".r
  protected val int: Regex = """[+-]?\d+""".r
  protected val identifier: Regex = """[a-zA-Z_][a-zA-Z_0-9]*""".r

  def expr: Parser[Expr] = operators(List(
    ("!", unary),
    ("+ -", unary),
    ("**", binary),
    ("* / %", binary),
    ("+ -", binary),
    ("== != >= <= > <", binary),
    ("&", binary),
    ("^", binary),
    ("|", binary)
  ))(exprFactor)

  def exprFactor: Parser[Expr] = exprBool | exprFun | exprIdent | exprFloat | exprInt | "(" ~> expr <~ ")"

  def exprBool: Parser[Expr] = exprTrue | exprFalse

  def exprTrue: Parser[Expr] = ExprParser.TRUE_VALUE ^^ { _ => ExprBool(true) }

  def exprFalse: Parser[Expr] = ExprParser.FALSE_VALUE ^^ { _ => ExprBool(false) }

  def exprInt: Parser[Expr] = int ^^ {
    value => ExprInt(value.toInt)
  }

  def exprFloat: Parser[Expr] = float ^^ {
    value => ExprFloat(value.toDouble)
  }

  def exprIdent: Parser[Expr] = identifier ^^ {
    name => ExprIdent(name)
  }

  def exprFun: Parser[Expr] = identifier ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case name ~ args => ExprFunction(name, args)
  }

  private def operators(ops: Seq[(String, (Parser[Expr], Parser[String]) => Parser[Expr])])
                       (s: Parser[Expr]): Parser[Expr] = {
    ops.foldLeft(s) { (p, op) => op._2(p, operexpr(op._1)) }
  }

  private def unary(p: Parser[Expr], op: Parser[String]): Parser[Expr] = opt(op) ~ p ^^ {
    case None ~ e => e
    case Some(o) ~ e => ExprFunction(o, List(e))
  }

  private def binary(p: Parser[Expr], op: Parser[String]): Parser[Expr] = {
    lazy val b: PackratParser[Expr] = opt(b ~ op) ~ p ^^ {
      case None ~ e => e
      case Some(l ~ o) ~ r => ExprFunction(o, List(l, r))
    }

    b
  }

  private def operexpr(ops: String): Parser[String] = ops.split("""\s+""").map(Parser(_)).reduce(_ | _)
}

object ExprParser {
  val TRUE_VALUE = "true"
  val FALSE_VALUE = "false"
}