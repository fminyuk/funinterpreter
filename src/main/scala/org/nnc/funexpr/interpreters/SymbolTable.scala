package org.nnc.funexpr.interpreters

trait SymbolTable {

  def getValue(ident: String): Option[Value]
}
