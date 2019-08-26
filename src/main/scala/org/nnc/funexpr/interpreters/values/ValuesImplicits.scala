package org.nnc.funexpr.interpreters.values

import org.nnc.funexpr.interpreters.types._

object ValuesImplicits {

  implicit val doubleConverter: ValuesConverter[Double] = new ValuesConverter[Double] {

    override val valueType: Type = DoubleType()

    override def encode(value: Double): Value = DoubleValue(value)

    override def decode(value: Value): Double = value.asInstanceOf[DoubleValue].value
  }

  implicit val boolConverter: ValuesConverter[Boolean] = new ValuesConverter[Boolean] {
    override val valueType: Type = BoolType()

    override def encode(value: Boolean): Value = BoolValue(value)

    override def decode(value: Value): Boolean = value.asInstanceOf[BoolValue].value
  }

  implicit def fun1Converter[T1, R](implicit
                                    t1: ValuesConverter[T1],
                                    r: ValuesConverter[R]): ValuesConverter[T1 => R] = {
    new ValuesConverter[T1 => R] {
      override val valueType: Type = FunctionType(r.valueType, Seq(t1.valueType))

      override def encode(value: T1 => R): Value = {
        FunctionValue {
          case Seq(a1) => r.encode(value(t1.decode(a1)))
        }
      }

      override def decode(value: Value): T1 => R = {
        val fun = value.asInstanceOf[FunctionValue].value

        a1 => r.decode(fun(Seq(t1.encode(a1))))
      }

    }
  }

  implicit def fun2Converter[T1, T2, R](implicit
                                        t1: ValuesConverter[T1],
                                        t2: ValuesConverter[T2],
                                        r: ValuesConverter[R]): ValuesConverter[(T1, T2) => R] = {
    new ValuesConverter[(T1, T2) => R] {
      override val valueType: Type = FunctionType(r.valueType, Seq(t1.valueType, t2.valueType))

      override def encode(value: (T1, T2) => R): Value = {
        FunctionValue {
          case Seq(a1, a2) => r.encode(value(t1.decode(a1), t2.decode(a2)))
        }
      }

      override def decode(value: Value): (T1, T2) => R = {
        val fun = value.asInstanceOf[FunctionValue].value

        (a1, a2) => r.decode(fun(Seq(t1.encode(a1), t2.encode(a2))))
      }
    }
  }

  implicit def fun3Converter[T1, T2, T3, R](implicit
                                            t1: ValuesConverter[T1],
                                            t2: ValuesConverter[T2],
                                            t3: ValuesConverter[T3],
                                            r: ValuesConverter[R]): ValuesConverter[(T1, T2, T3) => R] = {
    new ValuesConverter[(T1, T2, T3) => R] {
      override val valueType: Type = FunctionType(r.valueType, Seq(t1.valueType, t2.valueType, t3.valueType))

      override def encode(value: (T1, T2, T3) => R): Value = {
        FunctionValue {
          case Seq(a1, a2, a3) => r.encode(value(t1.decode(a1), t2.decode(a2), t3.decode(a3)))
        }
      }

      override def decode(value: Value): (T1, T2, T3) => R = {
        val fun = value.asInstanceOf[FunctionValue].value

        (a1, a2, a3) => r.decode(fun(Seq(t1.encode(a1), t2.encode(a2), t3.encode(a3))))
      }
    }
  }

  implicit def fun4Converter[T1, T2, T3, T4, R](implicit
                                                t1: ValuesConverter[T1],
                                                t2: ValuesConverter[T2],
                                                t3: ValuesConverter[T3],
                                                t4: ValuesConverter[T4],
                                                r: ValuesConverter[R]): ValuesConverter[(T1, T2, T3, T4) => R] = {
    new ValuesConverter[(T1, T2, T3, T4) => R] {
      override val valueType: Type = FunctionType(r.valueType, Seq(t1.valueType, t2.valueType, t3.valueType, t4.valueType))

      override def encode(value: (T1, T2, T3, T4) => R): Value = {
        FunctionValue {
          case Seq(a1, a2, a3, a4) => r.encode(value(t1.decode(a1), t2.decode(a2), t3.decode(a3), t4.decode(a4)))
        }
      }

      override def decode(value: Value): (T1, T2, T3, T4) => R = {
        val fun = value.asInstanceOf[FunctionValue].value

        (a1, a2, a3, a4) => r.decode(fun(Seq(t1.encode(a1), t2.encode(a2), t3.encode(a3), t4.encode(a4))))
      }
    }
  }
}
