package com.kbhit.playground.scala

trait CustomStream[+A] {

  def uncons: Option[(A, CustomStream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons.map((x) => x._1 :: x._2.toList).getOrElse(Nil)

}

object CustomStreamImpl {

  def empty[A]: CustomStream[A] =
    new CustomStream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => CustomStream[A]): CustomStream[A] =
    new CustomStream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): CustomStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
