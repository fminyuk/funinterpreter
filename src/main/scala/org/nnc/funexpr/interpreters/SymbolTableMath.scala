package org.nnc.funexpr.interpreters

class SymbolTableMath extends SymbolTableBase {

  import ValueCoderImplicits._

  private val funII = add[Int => Int] _
  private val funDD = add[Double => Double] _

  private val funIII = add[(Int, Int) => Int] _
  private val funDDD = add[(Double, Double) => Double] _

  add("pi")(math.Pi)
  add("e")(math.E)

  funII("abs")(_.abs)
  funII("sign")(_.sign)

  funIII("min")(_.min(_))
  funIII("max")(_.max(_))

  funDD("abs")(_.abs)
  funDD("sign")(_.sign)

  funDDD("min")(_.min(_))
  funDDD("max")(_.max(_))

  funDD("sqrt")(math.sqrt)
  funDD("cbrt")(math.cbrt)

  funDD("exp")(math.exp)
  funDD("log")(math.log)
  funDDD("pow")(math.pow)

  funDD("sin")(math.sin)
  funDD("cos")(math.cos)
  funDD("tan")(math.tan)
  funDD("cot")(1 / math.tan(_))
  funDD("asin")(math.asin)
  funDD("acos")(math.acos)
  funDD("atan")(math.atan)
  funDD("acot")(math.Pi / 2 - math.atan(_))

  funDD("sinh")(math.sinh)
  funDD("cosh")(math.cosh)
  funDD("tanh")(math.tanh)
  funDD("coth")(1 / math.tanh(_))
}
