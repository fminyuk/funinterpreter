package org.nnc.funexpr.interpreters

import scala.annotation.tailrec

class SymbolTableCompose(symbolTables: List[SymbolTable]) extends SymbolTable {
  override def getValue(ident: String): Option[Value] = {
    getValue(ident, symbolTables.head, symbolTables.tail)
  }

  @tailrec
  private def getValue(ident: String, head: SymbolTable, tail: List[SymbolTable]): Option[Value] = {
    head.getValue(ident) match {
      case None => tail match {
        case Nil => None
        case h :: t => getValue(ident, h, t)
      }
      case value => value
    }
  }
}
