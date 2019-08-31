package org.nnc.funexpr.interpreters

import org.nnc.funexpr.parsers.ExprParser
import org.scalatest.FunSuite

class ExprInterpreterTest extends FunSuite {
  private val parser = new ExprParser {}

  private val interpreter: ExprInterpreter = {
    import ValueCoderImplicits._

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
    s.add("==", (a: Double, b: Double) => a == b)
    s.add("!=", (a: Double, b: Double) => a != b)
    s.add(">=", (a: Double, b: Double) => a >= b)
    s.add("<=", (a: Double, b: Double) => a <= b)
    s.add(">", (a: Double, b: Double) => a > b)
    s.add("<", (a: Double, b: Double) => a < b)
    s.add("!", (a: Boolean) => !a)
    s.add("&&", (a: Boolean, b: Boolean) => a && b)
    s.add("||", (a: Boolean, b: Boolean) => a || b)
    s.add("dd", (f: (Double, Double) => Double, a: Double) => f(a, a))
    s.add("pow", (a: Double, b: Double) => math.pow(a, b))

    new ExprInterpreter(s)
  }

  test("identifier") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = interpreter.exec(e)

    assert(r == ValueItem[Double](0))
  }

  test("simple") {
    val e = parser.parseAll(parser.expr, "ten + 5").get

    val r = interpreter.exec(e)

    assert(r == ValueItem[Double](15))
  }

  test("complex") {
    val e = parser.parseAll(parser.expr, "(one + 2 + 3 * 2**3) / 9 + 1").get

    val r = interpreter.exec(e)

    assert(r == ValueItem[Double](4))
  }

//  test("case types") {
//    import scala.reflect.runtime.universe._
//
//    var r: NewValue= NewValueR[Int](2)
//    var t: NewValue= NewValueR[Int](3)
//
////    println(r.)
//    //    println(type(ExprType))
//  }
}
