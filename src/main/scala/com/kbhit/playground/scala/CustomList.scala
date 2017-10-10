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

  def sum2(l: CustomList[Int]): Double =
    foldRight(l, 0.0)(_ + _)

  def product2(l: CustomList[Double]): Double =
    foldRight(l, 1.0)(_ * _, t => t != 0)

  def tail[A](list: CustomList[A], count: Int = 1): CustomList[A] = (list, count) match {
    case (Cons(_, xs), _) if count > 0 => tail(xs, count - 1)
    case _ => list
  }

  def dropWhile[A](l: CustomList[A])(f: A => Boolean): CustomList[A] = l match {
    case Cons(head, rest) if f(head) => dropWhile(rest)(f)
    case _ => l
  }

  def length[A](l: CustomList[A]): Int =
    foldRight(l, 0)((_, b) => 1 + b)

  def reverse[A](l: CustomList[A]): CustomList[A] =
    foldLeft(l, CustomList[A]())((a: CustomList[A], b: A) => Cons(b, a))

  def apply[A](as: A*): CustomList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def map[A,B](l: CustomList[A])(f: A => B): CustomList[B] =
    foldRight(l, CustomList[B]())((b: A, a: CustomList[B]) => Cons(f(b), a))

  def filter[A](l: CustomList[A])(f: A => Boolean): CustomList[A] = l match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(_, xs) => filter(xs)(f)
  }

  def foldRight[A,B](l: CustomList[A], z: B)(f: (A, B) => B, g: (A) => Boolean = (_: A) => true): B =
    l match {
      case Nil => z
      case Cons(x, xs) if g(x) => f(x, foldRight(xs, z)(f, g))
      case Cons(x, _) => f(x, z)
    }

  def foldLeft[A,B](l: CustomList[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x))(f)
    }

}


