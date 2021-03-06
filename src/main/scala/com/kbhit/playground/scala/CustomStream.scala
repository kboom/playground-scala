package com.kbhit.playground.scala

import com.kbhit.playground.scala.CustomStreamImpl.unfold

trait CustomStream[+A] {

  def uncons: Option[(A, CustomStream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons.map((x) => x._1 :: x._2.toList).getOrElse(Nil)

  def take(n: Int): List[A] = if (n > 0) uncons.map((x) => x._1 :: x._2.take(n - 1)).getOrElse(Nil) else Nil

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

}

object CustomStreamImpl {

  def empty[A]: CustomStream[A] =
    new CustomStream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => CustomStream[A]): CustomStream[A] =
    new CustomStream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def fibs(): CustomStream[Int] =
    unfold((0, 0)) {
      case (0, 0) => Some((0, (0, 1)))
      case (0, _) => Some((1, (1, 1)))
      case (pp, pr) => Some((pr, (pr, pp + pr)))
    }

  def constant(c: Int): CustomStream[Int] =
    unfold(c)(_ => Some(c, c))

  def from(n: Int): CustomStream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): CustomStream[A] =
    new CustomStream[A] {
      lazy val uncons = f(z) match {
        case Some((h, t)) => Some((h, unfold(t)(f)))
        case _ => None
      }
    }

  def apply[A](as: A*): CustomStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
