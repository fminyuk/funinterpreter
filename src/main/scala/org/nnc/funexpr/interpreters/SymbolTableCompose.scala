package org.nnc.funexpr.interpreters

import scala.annotation.tailrec

class SymbolTableCompose(symbolTables: List[SymbolTable]) extends SymbolTable {
  override def getValue(ident: String): Option[Value] = {
    getValue(ident, symbolTables)
  }

  @tailrec
  private def getValue(ident: String, st: List[SymbolTable]): Option[Value] = {
    st match {
      case Nil => None
      case h :: t => h.getValue(ident) match {
        case None => getValue(ident, t)
        case value => value
      }
    }
  }
}
