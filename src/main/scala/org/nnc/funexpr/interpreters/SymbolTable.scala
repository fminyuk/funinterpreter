package org.nnc.funexpr.interpreters

import org.nnc.funexpr.interpreters.values.Value

trait SymbolTable {

  def getValue(ident: String): Value
}
