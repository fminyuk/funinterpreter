package org.nnc.funexpr.interpreters

import org.nnc.funexpr.ast.ExprIdent
import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSuite

import scala.reflect.runtime.universe._

class ExprInterpreterImplTest extends FunSuite with MockFactory {

  import ValueCoderImplicits._

  test("compiler call") {
    val e = ExprIdent("a")
    val p = new ExprProgram {
      override val res: Type = typeOf[Boolean]

      override def exec: Value = ValueItem(true)
    }

    val m = mock[ExprCompiler]
    (m.compile _).expects(e, typeOf[Boolean]).returns(Right(p))

    val interpreter = new ExprInterpreterImpl(m)

    val r = interpreter.exec[Boolean](e)

    assert(r == Right(true))
  }
}
