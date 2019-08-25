package org.nnc.funexpr.parsers

import org.nnc.funexpr.ast.{ExprFunction, ExprIdent, ExprValue}
import org.scalatest.FunSuite

class ExprParserTest extends FunSuite {

  private val p = new ExprParser {}

  test("value") {
    val r = p.parseAll(p.expr, "10")

    assert(r.successful)
    assert(r.get == ExprValue(10))
  }

  test("ident") {
    val r = p.parseAll(p.expr, "x")

    assert(r.successful)
    assert(r.get == ExprIdent("x"))
  }

  test("function") {
    val r = p.parseAll(p.expr, "min(4, a)")

    assert(r.successful)
    assert(r.get == ExprFunction("min", List(ExprValue(4), ExprIdent("a"))))
  }

  test("binary operators") {
    val r = p.parseAll(p.expr, "2 + 4 * 5")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprValue(2), ExprFunction("*", List(ExprValue(4), ExprValue(5))))))
  }

  test("binary operators + 2") {
    val r = p.parseAll(p.expr, "1 + 2")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprValue(1), ExprValue(2))))
  }

  test("binary operators + 3") {
    val r = p.parseAll(p.expr, "1 + 2 + 3")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprFunction("+", List(ExprValue(1), ExprValue(2))), ExprValue(3))))
  }

  test("binary operators + *") {
    val r = p.parseAll(p.expr, "1 + 2 * 3")

    assert(r.successful)
    assert(r.get == ExprFunction("+", List(ExprValue(1), ExprFunction("*", List(ExprValue(2), ExprValue(3))))))
  }

  test("unary operators") {
    val r = p.parseAll(p.expr, "-a * 4")

    assert(r.successful)
    assert(r.get == ExprFunction("*", List(ExprFunction("-", List(ExprIdent("a"))), ExprValue(4))))
  }
}
