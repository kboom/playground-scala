package com.kbhit.playground.scala

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2
    override def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2
    override def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2
    override def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 && a2
    override def zero = true
  }

}