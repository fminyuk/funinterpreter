package org.nnc.funexpr.interpreters

import org.nnc.funexpr.interpreters.values.{Value, ValuesConverter}

import scala.collection.mutable

class SymbolTableImpl extends SymbolTable {
  private val values: mutable.Map[String, Value] = new mutable.HashMap[String, Value]()

  override def getValue(ident: String): Value = values(ident)

  def add[T: ValuesConverter](ident: String, value: T): Unit = {
    values += ident -> implicitly[ValuesConverter[T]].encode(value)
  }
}
