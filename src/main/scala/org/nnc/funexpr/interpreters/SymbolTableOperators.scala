package org.nnc.funexpr.interpreters

class SymbolTableOperators extends SymbolTableBase {

  import ValueCoderImplicits._

  private val unaryBB = add[Boolean => Boolean] _
  private val unaryDD = add[Double => Double] _
  private val unaryII = add[Int => Int] _

  private val binaryBBB = add[(Boolean, Boolean) => Boolean] _
  private val binaryIII = add[(Int, Int) => Int] _
  private val binaryDDD = add[(Double, Double) => Double] _

  private val binaryIIB = add[(Int, Int) => Boolean] _
  private val binaryDDB = add[(Double, Double) => Boolean] _

  unaryBB("!")(!_)

  unaryII("+")(+_)
  unaryII("-")(+_)

  unaryDD("+")(+_)
  unaryDD("-")(+_)

  binaryBBB("^")(_ ^ _)
  binaryBBB("&")(_ && _)
  binaryBBB("|")(_ || _)

  binaryIII("*")(_ * _)
  binaryIII("/")(_ / _)
  binaryIII("%")(_ & _)
  binaryIII("+")(_ + _)
  binaryIII("-")(_ - _)

  binaryDDD("**")(math.pow)
  binaryDDD("*")(_ * _)
  binaryDDD("/")(_ / _)
  binaryDDD("+")(_ + _)
  binaryDDD("-")(_ - _)

  binaryIIB("==")(_ == _)
  binaryIIB("!=")(_ != _)
  binaryIIB(">=")(_ >= _)
  binaryIIB("<=")(_ <= _)
  binaryIIB(">")(_ > _)
  binaryIIB("<")(_ < _)

  binaryDDB("==")(_ == _)
  binaryDDB("!=")(_ != _)
  binaryDDB(">=")(_ >= _)
  binaryDDB("<=")(_ <= _)
  binaryDDB(">")(_ > _)
  binaryDDB("<")(_ < _)

  binaryBBB("==")(_ == _)
  binaryBBB("!=")(_ != _)
}
