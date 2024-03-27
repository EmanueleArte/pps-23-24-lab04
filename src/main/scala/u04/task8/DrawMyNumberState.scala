package u04.task8

import u04.task8.Optionals.*
import u04.task8.Optionals.Optional.*
import u04.task8.States.*

trait DrawMyNumberState:
  type Number

  def initialNumber(): Number

  def get(): State[Number, String]

  def set(v: String): State[Number, Unit]

  def nop(): State[Number, Unit]

def getNumberFromString(s: String): Optional[Int] = s.toLowerCase match
  case "zero" => Just(0)
  case "one" => Just(1)
  case "two" => Just(2)
  case "three" => Just(3)
  case "four" => Just(4)
  case "five" => Just(5)
  case "six" => Just(6)
  case "seven" => Just(7)
  case "eight" => Just(8)
  case "nine" => Just(9)
  case _ => Empty()

object DrawMyNumberStateImpl extends DrawMyNumberState:
  opaque type Number = Optional[Int]

  def initialNumber(): Number = Empty()

  def get(): State[Number, String] = State(i => (i, i match
    case Just(v) => "" + v
    case Empty() => ""
  ));

  def set(v: String): State[Number, Unit] = State(i => (getNumberFromString(v), ()));

  def nop(): State[Number, Unit] = State(i => (i, ()));
