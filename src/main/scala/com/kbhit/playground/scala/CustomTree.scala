package com.kbhit.playground.scala

sealed trait CustomTree[+A]

case class Leaf[+A](value: A) extends CustomTree[A]
case class Branch[+A](left: CustomTree[A], right: CustomTree[A]) extends CustomTree[A]

object CustomTree {

  def countLeaves[A](t: CustomTree[A]): Int =
    fold(t, 0)((x, y) => x + y, _ => 1)

  def maximum(t: CustomTree[Int]): Int =
    fold(t, Int.MinValue)((x, y) => x max y, x => x)

  def depth[A](t: CustomTree[A]): Int =
    fold(t, 0)((x, y) => (x max y) + 1, _ => 1)

  def map[A,B](t: CustomTree[A])(f: A => B): CustomTree[B] =
    fold(t, CustomTree[B]())((x, y) => Branch(x, y), x => Leaf(f(x)))

  def fold[A,B](t: CustomTree[A], z: B)(f: (B, B) => B, g: A => B): B = t match {
    case Branch(x, y) => f(fold(x, z)(f, g), fold(y, z)(f, g))
    case Leaf(x) => g(x)
    case _ => z
  }

  def apply[A](as: A*): CustomTree[A] =
    if (as.isEmpty) null
    else if (as.size == 1) Leaf(as.head)
    else Branch(apply(as.head), apply(as.last))

}
