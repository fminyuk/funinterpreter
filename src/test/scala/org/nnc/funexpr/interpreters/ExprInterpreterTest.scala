package org.nnc.funexpr.interpreters

import org.nnc.funexpr.parsers.ExprParser
import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class ExprInterpreterTest extends FunSuite {
  private val parser = new ExprParser {}

  private val interpreter: ExprInterpreter = {
    val symbolTable = new SymbolTableCompose(List(
      new SymbolTableOperators,
      new SymbolTableMath,
      new SymbolTableBase {

        import ValueCoderImplicits._

        add[Double]("zero")(0)
        add[Double]("one")(1)
        add[Double]("ten")(10)
      }
    ))

    new ExprInterpreter(symbolTable)
  }

  test("identifier") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = interpreter.exec(e)

    assert(r == Right(ValueItem[Double](0)))
  }

  test("simple") {
    val e = parser.parseAll(parser.expr, "ten + 5").get

    val r = interpreter.exec(e)

    assert(r == Right(ValueItem[Double](15)))
  }

  test("complex") {
    val e = parser.parseAll(parser.expr, "(one + 2 + 3 * 2**3) / 9 + 1").get

    val r = interpreter.exec(e)

    assert(r == Right(ValueItem[Double](4)))
  }

  test("identifier not found") {
    val e = parser.parseAll(parser.expr, "2 * two").get

    val r = interpreter.exec(e)

    assert(r == Left(ErrorIdentNotFound("two")))
  }

  test("identifier not function") {
    val e = parser.parseAll(parser.expr, "one(2)").get

    val r = interpreter.exec(e)

    assert(r == Left(ErrorIdentNotFunction("one")))
  }

  test("function arguments count mismatch") {
    val e = parser.parseAll(parser.expr, "min(1, 2, 3)").get

    val r = interpreter.exec(e)

    assert(r == Left(ErrorFunctionArgumentsMismatch(
      "min",
      Seq(typeOf[Double], typeOf[Double]),
      Seq(typeOf[Double], typeOf[Double], typeOf[Double])
    )))
  }

  test("function arguments types mismatch") {
    val e = parser.parseAll(parser.expr, "min(1, max)").get

    val r = interpreter.exec(e)

    assert(r == Left(ErrorFunctionArgumentsMismatch(
      "min",
      Seq(typeOf[Double], typeOf[Double]),
      Seq(typeOf[Double], typeOf[(Double, Double) => Double])
    )))
  }
}
