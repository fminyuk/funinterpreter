package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.{ExprBool, ExprFloat, ExprFunction, ExprIdent, ExprInt}
import org.scalatest.FunSuite

import scala.reflect.runtime.universe._

class ExprCompilerImplTest extends FunSuite {
  private val compiler = {
    val symbols = new SymbolTableCompose(List(
      new SymbolTableOperators,
      new SymbolTableMath,
      new SymbolTableBase {

        import ValueCoderImplicits._

        add[Boolean]("a")(false)
        add[Int]("a")(1)
        add[Double]("a")(2)
        add[() => Double]("a")(() => 3)

        add[(Int => Int, Int) => Int]("ff")((_, i) => i)
        add[(Double => Double, Int) => Int]("ff")((_, i) => i)
      }
    ))

    new ExprCompilerImpl(symbols)
  }

  test("value: bool") {
    val e = ExprBool(true)

    val r = compiler.compile(e, typeOf[Boolean])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(true)))
  }

  test("value: int") {
    val e = ExprInt(10)

    val r = compiler.compile(e, typeOf[Int])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(10)))
  }

  test("value: double") {
    val e = ExprFloat(10)

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(10.0)))
  }

  test("ident: bool") {
    val e = ExprIdent("a")

    val r = compiler.compile(e, typeOf[Boolean])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(false)))
  }

  test("ident: int") {
    val e = ExprIdent("a")

    val r = compiler.compile(e, typeOf[Int])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(1)))
  }

  test("ident: double") {
    val e = ExprIdent("a")

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(2.0)))
  }

  test("function: 0 args") {
    val e = ExprFunction("a", Seq())

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(3.0)))
  }

  test("function: 1 args") {
    val e = ExprFunction("abs", Seq(ExprFloat(-1)))

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(1.0)))
  }

  test("function: 2 args") {
    val e = ExprFunction("min", Seq(ExprFloat(2), ExprFloat(1)))

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(1.0)))
  }

  test("implicits converters") {
    val e = ExprFunction("+", Seq(ExprInt(2), ExprInt(2)))

    val r = compiler.compile(e, typeOf[Double])

    assert(r.map(_.exec) == Right(ValueItem(4.0)))
  }

  test("error: identifier not found") {
    val e = ExprIdent("aaa")

    val r = compiler.compile(e, typeOf[Double])

    assert(r == Left(ErrorIdentNotFound("aaa")))
  }

  test("error: function not found") {
    val e = ExprFunction("bbb", Seq())

    val r = compiler.compile(e, typeOf[Double])

    assert(r == Left(ErrorFunctionNotFound("bbb", Seq())))
  }

  test("error: expression wrong type") {
    val e = ExprFunction("&", Seq(ExprBool(true), ExprBool(false)))

    val r = compiler.compile(e, typeOf[Double])

    assert(r == Left(ErrorExprWrongType(e, typeOf[Double])))
  }

  test("error: expression ambiguous") {
    val e = ExprFunction("ff", Seq(ExprIdent("abs"), ExprInt(2)))

    val r = compiler.compile(e, typeOf[Int])

    assert(r == Left(ErrorExprAmbiguous(ExprFunction("ff", Seq(ExprIdent("abs"), ExprInt(2))), typeOf[Int])))
  }

  test("error: no suitable function") {
    val e = ExprFunction("min", Seq(ExprInt(2), ExprBool(false)))

    val r = compiler.compile(e, typeOf[Int])

    assert(r == Left(ErrorNoSuitableFunction("min", Seq(ExprInt(2), ExprBool(false)))))
  }

  test("complex") {
    val e = ExprFunction("**", Seq(ExprFloat(2), ExprFunction("min", Seq(ExprFloat(3), ExprFloat(1e4)))))

    val r = compiler.compile(e, typeOf[Double])

    assert(r.isRight)
    assert(r.map(_.exec) == Right(ValueItem(8.0)))
  }

  test("time") {
    val e = ExprFunction("**", Seq(ExprFloat(2), ExprFunction("min", Seq(ExprInt(3), ExprFloat(1e4)))))

    for (i <- 1 to 1000000) {
      val r = compiler.compile(e, typeOf[Double])

      assert(r.isRight)
      assert(r.map(_.exec) == Right(ValueItem(8.0)))
    }
  }
}
