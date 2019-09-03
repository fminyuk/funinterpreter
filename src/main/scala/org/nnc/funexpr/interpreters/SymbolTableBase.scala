package org.nnc.funexpr.interpreters

import scala.collection.mutable

abstract class SymbolTableBase extends SymbolTable {
  private val values: mutable.Map[String, Value] = new mutable.HashMap[String, Value]()

  override def getValue(ident: String): Option[Value] = values.get(ident)

  protected def add[T: ValueCoder](name: String)(value: T): Unit = {
    values += name -> implicitly[ValueCoder[T]].encode(value)
  }
}
