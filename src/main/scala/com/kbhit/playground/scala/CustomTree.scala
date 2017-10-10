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

}
