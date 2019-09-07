package org.nnc.funexpr.interpreters

import scala.reflect.runtime.universe._

object ValueCoderImplicits {

  implicit val doubleCoder: ValueCoder[Double] = new ValueCoder[Double] {
    override def encode(value: Double): Value = ValueItem[Double](value)

    override def decode(value: Value): Double = value.asInstanceOf[ValueItem[Double]].value
  }

  implicit val boolCoder: ValueCoder[Boolean] = new ValueCoder[Boolean] {
    override def encode(value: Boolean): Value = ValueItem[Boolean](value)

    override def decode(value: Value): Boolean = value.asInstanceOf[ValueItem[Boolean]].value
  }

  implicit def fun0Coder[R](implicit
                            rst: TypeTag[R],
                            rsc: ValueCoder[R]
                           ): ValueCoder[() => R] = {
    new ValueCoder[() => R] {
      override def encode(value: () => R): Value = ValueFunction[() => R] {
        case Seq() =>
          rsc.encode(value())
      }

      override def decode(value: Value): () => R = () => {
        val args = Seq(
        )
        rsc.decode(value.asInstanceOf[ValueFunction[() => R]].value(args))
      }
    }
  }

  implicit def fun1Coder[T1, R](implicit
                                t1t: TypeTag[T1],
                                t1c: ValueCoder[T1],
                                rst: TypeTag[R],
                                rsc: ValueCoder[R]): ValueCoder[(T1) => R] = {
    new ValueCoder[(T1) => R] {
      override def encode(value: (T1) => R): Value = ValueFunction[(T1) => R] {
        case Seq(a1) =>
          val v1 = t1c.decode(a1)
          rsc.encode(value(v1))
      }

      override def decode(value: Value): (T1) => R = (a1) => {
        val args = Seq(
          t1c.encode(a1)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1) => R]].value(args))
      }
    }
  }

  implicit def fun2Coder[T1, T2, R](implicit
                                    t1t: TypeTag[T1],
                                    t1c: ValueCoder[T1],
                                    t2t: TypeTag[T2],
                                    t2c: ValueCoder[T2],
                                    rst: TypeTag[R],
                                    rsc: ValueCoder[R]): ValueCoder[(T1, T2) => R] = {
    new ValueCoder[(T1, T2) => R] {
      override def encode(value: (T1, T2) => R): Value = ValueFunction[(T1, T2) => R] {
        case Seq(a1, a2) =>
          val v1 = t1c.decode(a1)
          val v2 = t2c.decode(a2)
          rsc.encode(value(v1, v2))
      }

      override def decode(value: Value): (T1, T2) => R = (a1, a2) => {
        val args = Seq(
          t1c.encode(a1),
          t2c.encode(a2)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1, T2) => R]].value(args))
      }
    }
  }

  implicit def fun3Coder[T1, T2, T3, R](implicit
                                        t1t: TypeTag[T1],
                                        t1c: ValueCoder[T1],
                                        t2t: TypeTag[T2],
                                        t2c: ValueCoder[T2],
                                        t3t: TypeTag[T3],
                                        t3c: ValueCoder[T3],
                                        rst: TypeTag[R],
                                        rsc: ValueCoder[R]): ValueCoder[(T1, T2, T3) => R] = {
    new ValueCoder[(T1, T2, T3) => R] {
      override def encode(value: (T1, T2, T3) => R): Value = ValueFunction[(T1, T2, T3) => R] {
        case Seq(a1, a2, a3) =>
          val v1 = t1c.decode(a1)
          val v2 = t2c.decode(a2)
          val v3 = t3c.decode(a3)
          rsc.encode(value(v1, v2, v3))
      }

      override def decode(value: Value): (T1, T2, T3) => R = (a1, a2, a3) => {
        val args = Seq(
          t1c.encode(a1),
          t2c.encode(a2),
          t3c.encode(a3)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1, T2, T3) => R]].value(args))
      }
    }
  }

  implicit def fun4Coder[T1, T2, T3, T4, R](implicit
                                            t1t: TypeTag[T1],
                                            t1c: ValueCoder[T1],
                                            t2t: TypeTag[T2],
                                            t2c: ValueCoder[T2],
                                            t3t: TypeTag[T3],
                                            t3c: ValueCoder[T3],
                                            t4t: TypeTag[T4],
                                            t4c: ValueCoder[T4],
                                            rst: TypeTag[R],
                                            rsc: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4) => R] = {
    new ValueCoder[(T1, T2, T3, T4) => R] {
      override def encode(value: (T1, T2, T3, T4) => R): Value = ValueFunction[(T1, T2, T3, T4) => R] {
        case Seq(a1, a2, a3, a4) =>
          val v1 = t1c.decode(a1)
          val v2 = t2c.decode(a2)
          val v3 = t3c.decode(a3)
          val v4 = t4c.decode(a4)
          rsc.encode(value(v1, v2, v3, v4))
      }

      override def decode(value: Value): (T1, T2, T3, T4) => R = (a1, a2, a3, a4) => {
        val args = Seq(
          t1c.encode(a1),
          t2c.encode(a2),
          t3c.encode(a3),
          t4c.encode(a4)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1, T2, T3, T4) => R]].value(args))
      }
    }
  }

  implicit def fun5Coder[T1, T2, T3, T4, T5, R](implicit
                                                t1t: TypeTag[T1],
                                                t1c: ValueCoder[T1],
                                                t2t: TypeTag[T2],
                                                t2c: ValueCoder[T2],
                                                t3t: TypeTag[T3],
                                                t3c: ValueCoder[T3],
                                                t4t: TypeTag[T4],
                                                t4c: ValueCoder[T4],
                                                t5t: TypeTag[T5],
                                                t5c: ValueCoder[T5],
                                                rst: TypeTag[R],
                                                rsc: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4, T5) => R] = {
    new ValueCoder[(T1, T2, T3, T4, T5) => R] {
      override def encode(value: (T1, T2, T3, T4, T5) => R): Value = ValueFunction[(T1, T2, T3, T4, T5) => R] {
        case Seq(a1, a2, a3, a4, a5) =>
          val v1 = t1c.decode(a1)
          val v2 = t2c.decode(a2)
          val v3 = t3c.decode(a3)
          val v4 = t4c.decode(a4)
          val v5 = t5c.decode(a5)
          rsc.encode(value(v1, v2, v3, v4, v5))
      }

      override def decode(value: Value): (T1, T2, T3, T4, T5) => R = (a1, a2, a3, a4, a5) => {
        val args = Seq(
          t1c.encode(a1),
          t2c.encode(a2),
          t3c.encode(a3),
          t4c.encode(a4),
          t5c.encode(a5)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1, T2, T3, T4, T5) => R]].value(args))
      }
    }
  }

  implicit def fun6Coder[T1, T2, T3, T4, T5, T6, R](implicit
                                                    t1t: TypeTag[T1],
                                                    t1c: ValueCoder[T1],
                                                    t2t: TypeTag[T2],
                                                    t2c: ValueCoder[T2],
                                                    t3t: TypeTag[T3],
                                                    t3c: ValueCoder[T3],
                                                    t4t: TypeTag[T4],
                                                    t4c: ValueCoder[T4],
                                                    t5t: TypeTag[T5],
                                                    t5c: ValueCoder[T5],
                                                    t6t: TypeTag[T6],
                                                    t6c: ValueCoder[T6],
                                                    rst: TypeTag[R],
                                                    rsc: ValueCoder[R]): ValueCoder[(T1, T2, T3, T4, T5, T6) => R] = {
    new ValueCoder[(T1, T2, T3, T4, T5, T6) => R] {
      override def encode(value: (T1, T2, T3, T4, T5, T6) => R): Value = ValueFunction[(T1, T2, T3, T4, T5, T6) => R] {
        case Seq(a1, a2, a3, a4, a5, a6) =>
          val v1 = t1c.decode(a1)
          val v2 = t2c.decode(a2)
          val v3 = t3c.decode(a3)
          val v4 = t4c.decode(a4)
          val v5 = t5c.decode(a5)
          val v6 = t6c.decode(a6)
          rsc.encode(value(v1, v2, v3, v4, v5, v6))
      }

      override def decode(value: Value): (T1, T2, T3, T4, T5, T6) => R = (a1, a2, a3, a4, a5, a6) => {
        val args = Seq(
          t1c.encode(a1),
          t2c.encode(a2),
          t3c.encode(a3),
          t4c.encode(a4),
          t5c.encode(a5),
          t6c.encode(a6)
        )
        rsc.decode(value.asInstanceOf[ValueFunction[(T1, T2, T3, T4, T5, T6) => R]].value(args))
      }
    }
  }
}
