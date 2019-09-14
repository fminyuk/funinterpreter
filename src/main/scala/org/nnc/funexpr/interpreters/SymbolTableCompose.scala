package org.nnc.funexpr.interpreters

class SymbolTableCompose(symbols: Iterable[SymbolTable]) extends SymbolTable {
  override def getValue(ident: String): Seq[Value] = {
    symbols.flatMap(_.getValue(ident)).toSeq
  }
}
