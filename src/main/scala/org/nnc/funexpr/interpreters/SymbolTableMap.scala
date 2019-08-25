package org.nnc.funexpr.interpreters

import org.nnc.funexpr.interpreters.values.Value

class SymbolTableMap(values: Map[String, Value]) extends SymbolTable {

  override def getValue(ident: String): Value = values(ident)
}
