package org.nnc.funexpr.interpreters

import scala.collection.mutable

class SymbolTableImpl extends SymbolTable {
  private val values: mutable.Map[String, Value] = new mutable.HashMap[String, Value]()

  override def getValue(ident: String): Option[Value] = values.get(ident)

  def add[T: ValuesConverter](ident: String, value: T): Unit = {
    values += ident -> implicitly[ValuesConverter[T]].encode(value)
  }
}
