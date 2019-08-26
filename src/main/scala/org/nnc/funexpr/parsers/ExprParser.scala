package org.nnc.funexpr.parsers

import org.nnc.funexpr.ast._

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

trait ExprParser extends RegexParsers with PackratParsers {

  private val real = """[+-]?\d+(:?\.\d*)?(:?[eE][+-]?\d+)?""".r

  private val identifier = """[a-zA-Z_][a-zA-Z_0-9]*""".r

  lazy val expr: Parser[Expr] = operators(List(
    ("!", unary),
    ("+ -", unary),
    ("**", binary),
    ("* /", binary),
    ("+ -", binary),
    ("== != >= <= > <", binary),
    ("&&", binary),
    ("||", binary)
  ))(exprFactor)

  lazy val exprFactor: Parser[Expr] = exprFun | exprIdent | exprValue | "(" ~> expr <~ ")"

  lazy val exprValue: Parser[Expr] = real ^^ {
    value => ExprValue(value.toDouble)
  }

  lazy val exprIdent: Parser[Expr] = identifier ^^ {
    name => ExprIdent(name)
  }

  lazy val exprFun: Parser[Expr] = identifier ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ {
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
