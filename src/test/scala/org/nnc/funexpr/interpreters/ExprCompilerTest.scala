package org.nnc.funexpr.interpreters

import org.nnc.funexpr.parsers.ExprParser
import org.scalatest.FunSuite

import scala.util.Random

class ExprCompilerTest extends FunSuite {
  private val parser = new ExprParser {}
  private val compiler = {
    val symbols = new SymbolTableCompose(List(
      new SymbolTableOperators,
      new SymbolTableMath,
      new SymbolTableBase {

        import ValueCoderImplicits._

        val rand = new Random()

        add[Double]("zero")(0)
        add[Double]("one")(1)
        add[Double]("ten")(10)

        add[Double]("a")(10)
        add[Int]("a")(10)

        add[() => Double]("rand")(rand.nextDouble)
      }
    ))

    new ExprCompiler(symbols)
  }

  test("value") {
    val e = parser.parseAll(parser.expr, "10").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }

  test("ident: one") {
    val e = parser.parseAll(parser.expr, "zero").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }

  test("ident: two") {
    val e = parser.parseAll(parser.expr, "a").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 2)
  }

  test("ident: not found") {
    val e = parser.parseAll(parser.expr, "some").get

    val r = compiler.compile(e)

    assert(r == Left(ErrorIdentNotFound("some")))
  }

  test("function: 0 args") {
    val e = parser.parseAll(parser.expr, "rand()").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }

  test("function: 1 args") {
    val e = parser.parseAll(parser.expr, "abs(1f)").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }

  test("function: 2 args") {
    val e = parser.parseAll(parser.expr, "min(2f, 1f)").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }

  test("complex") {
    val e = parser.parseAll(parser.expr, "2f ** min(3f, 1e4)").get

    val r = compiler.compile(e)

    assert(r.isRight)
    assert(r.map(_.size).getOrElse(0) == 1)
  }
}
