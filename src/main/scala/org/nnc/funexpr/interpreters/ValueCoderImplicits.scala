package org.nnc.funexpr.interpreters

object ValueCoderImplicits {

  implicit val doubleCoder: ValueCoder[Double] = new ValueCoder[Double] {

    override def encode(value: Double): Value = ValueItem[Double](value)

    override def decode(value: Value): Double = value.asInstanceOf[ValueItem[Double]].value
  }

  implicit val boolCoder: ValueCoder[Boolean] = new ValueCoder[Boolean] {
    override def encode(value: Boolean): Value = ValueItem[Boolean](value)

    override def decode(value: Value): Boolean = value.asInstanceOf[ValueItem[Boolean]].value
  }

  implicit def fun1Coder[T1, R](implicit
                                t1: ValueCoder[T1],
                                r: ValueCoder[R]): ValueCoder[(T1) => R] = {
    new ValueCoder[(T1) => R] {
      override def encode(value: (T1) => R): Value = ValueFunction {
        case Seq(a1) =>
          val v1 = t1.decode(a1)
          r.encode(value(v1))
      }

      override def decode(value: Value): (T1) => R = (a1) => {
        val args = Seq(
          t1.encode(a1)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }

  implicit def fun2Coder[T1, T2, R](implicit
                                    t1: ValueCoder[T1],
                                    t2: ValueCoder[T2],
                                    r: ValueCoder[R]): ValueCoder[(T1, T2) => R] = {
    new ValueCoder[(T1, T2) => R] {
      override def encode(value: (T1, T2) => R): Value = ValueFunction {
        case Seq(a1, a2) =>
          val v1 = t1.decode(a1)
          val v2 = t2.decode(a2)
          r.encode(value(v1, v2))
      }

      override def decode(value: Value): (T1, T2) => R = (a1, a2) => {
        val args = Seq(
          t1.encode(a1),
          t2.encode(a2)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }

  implicit def fun3Coder[T1, T2, T3, R](implicit
                                        t1: ValueCoder[T1],
                                        t2: ValueCoder[T2],
                                        t3: ValueCoder[T3],
                                        r: ValueCoder[R]): ValueCoder[(T1, T2, T3) => R] = {
    new ValueCoder[(T1, T2, T3) => R] {
      override def encode(value: (T1, T2, T3) => R): Value = ValueFunction {
        case Seq(a1, a2, a3) =>
          val v1 = t1.decode(a1)
          val v2 = t2.decode(a2)
          val v3 = t3.decode(a3)
          r.encode(value(v1, v2, v3))
      }

      override def decode(value: Value): (T1, T2, T3) => R = (a1, a2, a3) => {
        val args = Seq(
          t1.encode(a1),
          t2.encode(a2),
          t3.encode(a3)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }

  implicit def fun4Coder[T1, T2, T3, T4, R](implicit
                                            t1: ValueCoder[T1],
                                            t2: ValueCoder[T2],
                                            t3: ValueCoder[T3],
                                            t4: ValueCoder[T4],
                                            r: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4) => R] = {
    new ValueCoder[(T1, T2, T3, T4) => R] {
      override def encode(value: (T1, T2, T3, T4) => R): Value = ValueFunction {
        case Seq(a1, a2, a3, a4) =>
          val v1 = t1.decode(a1)
          val v2 = t2.decode(a2)
          val v3 = t3.decode(a3)
          val v4 = t4.decode(a4)
          r.encode(value(v1, v2, v3, v4))
      }

      override def decode(value: Value): (T1, T2, T3, T4) => R = (a1, a2, a3, a4) => {
        val args = Seq(
          t1.encode(a1),
          t2.encode(a2),
          t3.encode(a3),
          t4.encode(a4)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }

  implicit def fun5Coder[T1, T2, T3, T4, T5, R](implicit
                                                t1: ValueCoder[T1],
                                                t2: ValueCoder[T2],
                                                t3: ValueCoder[T3],
                                                t4: ValueCoder[T4],
                                                t5: ValueCoder[T5],
                                                r: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4, T5) => R] = {
    new ValueCoder[(T1, T2, T3, T4, T5) => R] {
      override def encode(value: (T1, T2, T3, T4, T5) => R): Value = ValueFunction {
        case Seq(a1, a2, a3, a4, a5) =>
          val v1 = t1.decode(a1)
          val v2 = t2.decode(a2)
          val v3 = t3.decode(a3)
          val v4 = t4.decode(a4)
          val v5 = t5.decode(a5)
          r.encode(value(v1, v2, v3, v4, v5))
      }

      override def decode(value: Value): (T1, T2, T3, T4, T5) => R = (a1, a2, a3, a4, a5) => {
        val args = Seq(
          t1.encode(a1),
          t2.encode(a2),
          t3.encode(a3),
          t4.encode(a4),
          t5.encode(a5)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }

  implicit def fun6Coder[T1, T2, T3, T4, T5, T6, R](implicit
                                                    t1: ValueCoder[T1],
                                                    t2: ValueCoder[T2],
                                                    t3: ValueCoder[T3],
                                                    t4: ValueCoder[T4],
                                                    t5: ValueCoder[T5],
                                                    t6: ValueCoder[T6],
                                                    r: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4, T5, T6) => R] = {
    new ValueCoder[(T1, T2, T3, T4, T5, T6) => R] {
      override def encode(value: (T1, T2, T3, T4, T5, T6) => R): Value = ValueFunction {
        case Seq(a1, a2, a3, a4, a5, a6) =>
          val v1 = t1.decode(a1)
          val v2 = t2.decode(a2)
          val v3 = t3.decode(a3)
          val v4 = t4.decode(a4)
          val v5 = t5.decode(a5)
          val v6 = t6.decode(a6)
          r.encode(value(v1, v2, v3, v4, v5, v6))
      }

      override def decode(value: Value): (T1, T2, T3, T4, T5, T6) => R = (a1, a2, a3, a4, a5, a6) => {
        val args = Seq(
          t1.encode(a1),
          t2.encode(a2),
          t3.encode(a3),
          t4.encode(a4),
          t5.encode(a5),
          t6.encode(a6)
        )
        r.decode(value.asInstanceOf[ValueFunction].value(args))
      }
    }
  }
}
