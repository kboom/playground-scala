package com.kbhit.playground.scala

sealed trait CustomList[+A]

case object Nil extends CustomList[Nothing]

case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList {

  def append[A](a1: CustomList[A], a2: CustomList[A]): CustomList[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def setHead[A](l: CustomList[A], e: A): CustomList[A] = Cons(e, l)

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

  def dropWhile[A](l: CustomList[A])(f: A => Boolean): CustomList[A] = l match {
    case Cons(head, rest) if f(head) => dropWhile(rest)(f)
    case _ => l
  }

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}


