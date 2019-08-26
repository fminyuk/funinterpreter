package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.ExprValue
import org.nnc.funexpr.interpreters.values.DoubleValue
import org.nnc.funexpr.parsers.ExprParser
import org.scalatest.FunSuite

class ExprInterpreterTest extends FunSuite {
  private val parser = new ExprParser {}

  private val interpreter: ExprInterpreter = {
    import values.ValuesImplicits._

    val s = new SymbolTableImpl()

    s.add("zero", 0.0)
    s.add("one", 1.0)
    s.add("ten", 10.0)
    s.add("+", (a: Double) => +a)
    s.add("-", (a: Double) => -a)
    s.add("**", (a: Double, b: Double) => math.pow(a, b))
    s.add("*", (a: Double, b: Double) => a * b)
    s.add("/", (a: Double, b: Double) => a / b)
    s.add("+", (a: Double, b: Double) => a + b)
    s.add("-", (a: Double, b: Double) => a - b)

    new ExprInterpreter(s)
  }

  test("identifier") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = interpreter.exec(e)

    assert(r == DoubleValue(0))
  }

  test("simple") {
    val e = parser.parseAll(parser.expr, "ten + 5").get

    val r = interpreter.exec(e)

    assert(r == DoubleValue(15))
  }

  test("complex") {
    val e = parser.parseAll(parser.expr, "(one + 2 + 3 * 2**3) / 9 + 1").get

    val r = interpreter.exec(e)

    assert(r == DoubleValue(4))
  }
}
