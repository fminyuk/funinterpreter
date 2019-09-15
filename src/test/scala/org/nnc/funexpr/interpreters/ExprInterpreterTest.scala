package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe._
import org.nnc.funexpr.parsers.ExprParser
import org.scalatest.FunSuite

class ExprInterpreterTest extends FunSuite {

  import ValueCoderImplicits._

  private val parser = new ExprParser {}
  private val compiler = {
    val symbolTable = new SymbolTableCompose(List(
      new SymbolTableOperators,
      new SymbolTableMath,
      new SymbolTableBase {
        add[Double]("zero")(0)
        add[Int]("zero")(0)

        add[(Double => Double, Int) => Int]("ff")((_, i) => i)
        add[(Int => Int, Int) => Int]("ff")((_, i) => i)

        add[() => Int]("fun")(() => 1)
      }
    ))

    new ExprCompiler(symbolTable)
  }
  private val interpreter = new ExprInterpreter(compiler)

  test("value: bool") {
    val e = parser.parseAll(parser.expr, "true").get

    val r = interpreter.exec[Boolean](e)

    assert(r == Right(true))
  }

  test("value: int") {
    val e = parser.parseAll(parser.expr, "10").get

    val r = interpreter.exec[Int](e)

    assert(r == Right(10))
  }

  test("value: double") {
    val e = parser.parseAll(parser.expr, "10f").get

    val r = interpreter.exec[Double](e)

    assert(r == Right(10))
  }

  test("ident: int") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = interpreter.exec[Int](e)

    assert(r == Right(0))
  }

  test("ident: double") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = interpreter.exec[Double](e)

    assert(r == Right(0))
  }

  test("function: 0 args") {
    val e = parser.parseAll(parser.expr, "fun()").get

    val r = interpreter.exec[Int](e)

    assert(r == Right(1))
  }

  test("function: 1 args") {
    val e = parser.parseAll(parser.expr, "abs(-10f)").get

    val r = interpreter.exec[Double](e)

    assert(r == Right(10))
  }

  test("function: 2 args") {
    val e = parser.parseAll(parser.expr, "max(1f, 1e3)").get

    val r = interpreter.exec[Double](e)

    assert(r == Right(1000))
  }

  test("complex") {
    val e = parser.parseAll(parser.expr, "9 + 4 * 5 < 30").get

    val r = interpreter.exec[Boolean](e)

    assert(r == Right(true))
  }

  test("identifier not found") {
    val e = parser.parseAll(parser.expr, "aaa").get

    val r = interpreter.exec[Double](e)

    assert(r == Left(ErrorIdentNotFound("aaa")))
  }

  test("function not found") {
    val e = parser.parseAll(parser.expr, "bbb()").get

    val r = interpreter.exec[Double](e)

    assert(r == Left(ErrorFunctionNotFound("bbb")))
  }

  test("expression wrong type") {
    val e = parser.parseAll(parser.expr, "2 + 2").get

    val r = interpreter.exec[Double](e)

    assert(r == Left(ErrorExprWrongType(typeOf[Double])))
  }

  test("expression ambiguous") {
    val e = parser.parseAll(parser.expr, "ff(abs, 2)").get

    val r = interpreter.exec[Int](e)

    assert(r == Left(ErrorExprAmbiguous(typeOf[Int])))
  }
}
