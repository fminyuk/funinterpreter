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

  private val convBB = add[Boolean => Boolean] _
  private val convIB = add[Int => Boolean] _
  private val convDB = add[Double => Boolean] _

  private val convBI = add[Boolean => Int] _
  private val convII = add[Int => Int] _
  private val convDI = add[Double => Int] _

  private val convBD = add[Boolean => Double] _
  private val convID = add[Int => Double] _
  private val convDD = add[Double => Double] _

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

  convBB("bool")(a => a)
  convIB("bool")(_ == 0)
  convDB("bool")(_ == 0.0)

  convBI("int")(if (_) 1 else 0)
  convII("int")(a => a)
  convDI("int")(_.intValue())

  convBD("float")(if (_) 1.0 else 0.0)
  convID("float")(_.doubleValue())
  convDD("float")(a => a)

  convID(SymbolTable.IMPLICIT)(_.doubleValue())
}
