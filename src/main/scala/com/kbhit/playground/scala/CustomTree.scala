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

  def depth[A](t: CustomTree[A]): Int = t match {
    case Branch(x, y) => (depth(x) max depth(y)) + 1
    case null => 0
    case Leaf(_) => 1
  }

}
