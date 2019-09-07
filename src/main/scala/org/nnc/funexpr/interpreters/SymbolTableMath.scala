package org.nnc.funexpr.interpreters

class SymbolTableMath extends SymbolTableBase {

  import ValueCoderImplicits._

  add("pi")(math.Pi)
  add("e")(math.E)

  add[Double => Double]("sqrt")(math.sqrt)
  add[Double => Double]("cbrt")(math.cbrt)

  add[Double => Double]("exp")(math.exp)
  add[Double => Double]("log")(math.log)
  add[(Double, Double) => Double]("pow")(math.pow)

  add[Double => Double]("sin")(math.sin)
  add[Double => Double]("cos")(math.cos)
  add[Double => Double]("tan")(math.tan)
  add[Double => Double]("cot")(1 / math.tan(_))
  add[Double => Double]("asin")(math.asin)
  add[Double => Double]("acos")(math.acos)
  add[Double => Double]("atan")(math.atan)
  add[Double => Double]("acot")(math.Pi / 2 - math.atan(_))

  add[Double => Double]("sinh")(math.sinh)
  add[Double => Double]("cosh")(math.cosh)
  add[Double => Double]("tanh")(math.tanh)
  add[Double => Double]("coth")(1 / math.tanh(_))

  add[(Double, Double) => Double]("min")(_.min(_))
  add[(Double, Double) => Double]("max")(_.max(_))

  add[Double => Double]("abs")(_.abs)
  add[Double => Double]("sign")(_.sign)
}
