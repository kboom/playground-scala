package com.kbhit.playground.scala

sealed trait CustomList[+A]

private case object CustomNil extends CustomList[Nothing]

case class Cons[+A](head: A, tail: CustomList[A]) extends CustomList[A]

object CustomList {

  def append[A](a1: CustomList[A], a2: CustomList[A]): CustomList[A] =
    a1 match {
      case CustomNil => a2
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
    if (as.isEmpty) CustomNil
    else Cons(as.head, apply(as.tail: _*))

  def map[A,B](l: CustomList[A])(f: A => B): CustomList[B] =
    foldRight(l, CustomList[B]())((b: A, a: CustomList[B]) => Cons(f(b), a))

  def flatMap[A,B](l: CustomList[A])(f: A => CustomList[B]): CustomList[B] = l match {
    case CustomNil => CustomNil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def sumElements(a: CustomList[Int], b: CustomList[Int]): CustomList[Int] =
    mapPairwise(a, b)(_ + _)

  def multiplyElements(a: CustomList[Int], b: CustomList[Int]): CustomList[Int] =
    mapPairwise(a, b)(_ * _)

  def mapPairwise[A](a: CustomList[A], b: CustomList[A])(f: (A, A) => A): CustomList[A] = (a, b) match {
    case (CustomNil, CustomNil) => CustomNil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), mapPairwise(xs, ys)(f))
    case _ => throw new IllegalArgumentException("Both lists should be of the same size")
  }

  def filter[A](l: CustomList[A])(f: A => Boolean): CustomList[A] =
    flatMap(l)(t => if (f(t)) CustomList(t) else CustomNil)

  def foldRight[A,B](l: CustomList[A], z: B)(f: (A, B) => B, g: (A) => Boolean = (_: A) => true): B =
    l match {
      case CustomNil => z
      case Cons(x, xs) if g(x) => f(x, foldRight(xs, z)(f, g))
      case Cons(x, _) => f(x, z)
    }

  def foldLeft[A,B](l: CustomList[A], z: B)(f: (B, A) => B): B =
    l match {
      case CustomNil => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x))(f)
    }

}


