package org.nnc.funexpr.interpreters

import scala.collection.mutable

abstract class SymbolTableBase extends SymbolTable {
  private val values: mutable.Map[String, Seq[Value]] = new mutable.HashMap[String, Seq[Value]]()

  override def getValue(ident: String): Seq[Value] = values.getOrElse(ident, Seq())

  protected def add[T: ValueCoder](name: String)(value: T): Unit = {
    val seq = values.getOrElse(name, Seq())
    values(name) = seq :+ implicitly[ValueCoder[T]].encode(value)
  }
}
