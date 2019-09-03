package org.nnc.funexpr.interpreters

class SymbolTableOperators extends SymbolTableBase {

  import ValueCoderImplicits._

  private val unaryDD = add[(Double) => Double] _
  private val unaryBB = add[(Boolean) => Boolean] _

  private val binaryDDD = add[(Double, Double) => Double] _
  private val binaryDDB = add[(Double, Double) => Boolean] _
  private val binaryBBB = add[(Boolean, Boolean) => Boolean] _

  unaryDD("+")(+_)
  unaryDD("-")(+_)

  binaryDDD("**")(math.pow)
  binaryDDD("*")(_ * _)
  binaryDDD("/")(_ / _)
  binaryDDD("+")(_ + _)
  binaryDDD("-")(_ - _)

  binaryDDB("==")(_ == _)
  binaryDDB("!=")(_ != _)
  binaryDDB(">=")(_ >= _)
  binaryDDB("<=")(_ <= _)
  binaryDDB(">")(_ > _)
  binaryDDB("<")(_ < _)

  unaryBB("!")(!_)

  binaryBBB("&&")(_ && _)
  binaryBBB("||")(_ || _)
}
