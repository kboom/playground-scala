package com.kbhit.playground.scala.fsm

/**
  * A candy vending machine FSM
  *
  * The Machine has two types of input
  *  - you can insert a coin
  *  - you can turn the knob
  *
  * The Machine can be in one of two states
  *  - locked
  *  - unlocked
  *
  * It also tracks how many candies are left and how many coins it contains
  *
  * The rules are as follows:
  *
  * - Inserting a coin into a locked machine will cause it to unlock if there's any candy left.
  * - Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  * - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  * - A machine that’s out of candy ignores all inputs.
  *
  * This is taken from [Functional Programming in Scala](http://www.manning.com/bjarnason/)
  */
object SimpleCandyDispenser {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  sealed trait Machine {
    def candies: Int
    def coins: Int
  }
  object Machine {
    def apply(candies: Int, coins: Int): Machine = LockedMachine(candies, coins)
  }
  case class LockedMachine(candies: Int, coins: Int) extends Machine
  case class UnlockedMachine(candies: Int, coins: Int) extends Machine

  val fsm =
    Fsm[Input, Machine] {
      case (Coin, LockedMachine(candies, coins)) if candies > 0 =>
        UnlockedMachine(candies, coins + 1)

      case (Turn, UnlockedMachine(candies, coins)) if candies > 0 =>
        LockedMachine(candies - 1, coins)
    }
}
