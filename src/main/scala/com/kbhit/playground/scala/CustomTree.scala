package com.kbhit.playground.scala

sealed trait CustomTree[+A]

case class Leaf[+A](value: A) extends CustomTree[A]
case class Branch[+A](left: CustomTree[A], right: CustomTree[A]) extends CustomTree[A]

object CustomTree {

  def countLeaves[A](t: CustomTree[A]): Int = t match {
    case null => 0
    case Branch(left, right) => countLeaves(left) + countLeaves(right)
    case Leaf(_) => 1
  }

  def maximum(t: CustomTree[Int]): Int = t match {
    case Branch(x, y) => maximum(x) max maximum(y)
    case Leaf(x) => x
    case null => Int.MinValue
  }

  def depth[A](t: CustomTree[A]): Int =
    fold(t, 0)((x, y) => (x max y) + 1)

  def map[A,B](t: CustomTree[A])(f: A => B): CustomTree[B] = t match {
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    case Leaf(x) => Leaf(f(x))
    case _ => null
  }

  def fold[A,B](t: CustomTree[A], z: B)(f: (B, B) => B): B = t match {
    case Branch(x, y) => f(fold(x, z)(f), fold(y, z)(f))
    case Leaf(_) => f(z, z)
    case _ => z
  }

}
