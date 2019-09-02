package org.nnc.funexpr.interpreters

import scala.annotation.tailrec

class SymbolTableCompose(symbolTables: List[SymbolTable]) extends SymbolTable {
  override def getValue(ident: String): Option[Value] = {
    SymbolTableCompose.getValue(ident, symbolTables)
  }
}

object SymbolTableCompose {
  @tailrec
  private def getValue(ident: String, symbolTables: List[SymbolTable]): Option[Value] = {
    symbolTables match {
      case Nil => None
      case h :: t => h.getValue(ident) match {
        case None => getValue(ident, t)
        case value => value
      }
    }
  }
}
