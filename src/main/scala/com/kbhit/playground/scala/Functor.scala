package com.kbhit.playground.scala

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
}

object listFunctor extends Functor[List] {
  def map[A, B](as: List[A])(f: A => B): List[B] = as map f
}
