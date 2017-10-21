package com.kbhit.playground.scala

import com.kbhit.playground.scala.Parallel.Par

trait Monad[M[_]] extends Functor[M] {

  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]]
    = traverse(lma)(fa => fa)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]]
    = la.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

}

object Monad {

  val parMonad = new Monad[Par] {

    def unit[A](a: => A): Par[A]
    = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B]
    = Par.flatMap(ma)(f)

    private def toJFunction[A, B](f: A => B) =
      new java.util.function.Function[A, B] {
        def apply(a: A) = f(a)
      }

  }

}