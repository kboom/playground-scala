package com.kbhit.playground.scala

sealed trait CustomList[+A]

case object Nil extends CustomList[Nothing]

case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList {

  def sum(ints: CustomList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: CustomList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: CustomList[A], count: Int = 1): CustomList[A] = (list, count) match {
    case (Cons(_, xs), _) if count > 0 => tail(xs, count - 1)
    case _ => list
  }

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}


