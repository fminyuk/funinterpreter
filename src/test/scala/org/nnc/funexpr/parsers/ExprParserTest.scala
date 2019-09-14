package org.nnc.funexpr.parsers

import org.nnc.funexpr.ast._
import org.scalatest.FunSuite

class ExprParserTest extends FunSuite {

  private val p = new ExprParser {}

  test("value: bool") {
    val r = p.parseAll(p.expr, "true")

    assert(r.successful)
    assert(r.get == ExprBool(true))
  }

  test("value: integer") {
    val r = p.parseAll(p.expr, "10")

    assert(r.successful)
    assert(r.get == ExprInt(10))
  }

  test("value: float") {
    val r = p.parseAll(p.expr, "10f")

    assert(r.successful)
    assert(r.get == ExprFloat(10))
  }

  test("ident") {
    val r = p.parseAll(p.expr, "x")

    assert(r.successful)
    assert(r.get == ExprIdent("x"))
  }

  test("function: 0 args") {
    val r = p.parseAll(p.expr, "rand()")

    assert(r.successful)
    assert(r.get == ExprFunction("rand", List()))
  }

  test("function: 1 args") {
    val r = p.parseAll(p.expr, "f(a)")

    assert(r.successful)
    assert(r.get == ExprFunction("f", List(ExprIdent("a"))))
  }

  test("function: 2 args") {
    val r = p.parseAll(p.expr, "min(3,b)")

    assert(r.successful)
    assert(r.get == ExprFunction("min", List(ExprInt(3), ExprIdent("b"))))
  }

  test("unary operators") {
    val r = p.parseAll(p.expr, "-a")

    assert(r.successful)
    assert(r.get == ExprFunction("-", List(ExprIdent("a"))))
  }

  test("binary operators: a + b") {
    val r = p.parseAll(p.expr, "a + b")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprIdent("a"), ExprIdent("b"))))
  }

  test("binary operators: a + b + c") {
    val r = p.parseAll(p.expr, "a + b + c")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprFunction("+", List(ExprIdent("a"), ExprIdent("b"))), ExprIdent("c"))))
  }

  test("binary operators: a + b * c") {
    val r = p.parseAll(p.expr, "a + b * c")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprIdent("a"), ExprFunction("*", List(ExprIdent("b"), ExprIdent("c"))))))
  }
}
