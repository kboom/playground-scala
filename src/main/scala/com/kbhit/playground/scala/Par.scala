package com.kbhit.playground.scala

import java.util.concurrent.{CompletableFuture, ExecutorService, Future}
import java.util.function.BiFunction

import com.kbhit.playground.scala.Parallel.Par

package object Parallel {
  type Par[A] = ExecutorService => CompletableFuture[A]
}

object Par {

  def run[A](s: ExecutorService)(a: Par[A]): CompletableFuture[A] = a(s)

  def unit[A](a: A): Par[A] = (_) => CompletableFuture.completedFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    (s) => a(s)

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B]
  = (a) => async(f(a))

  def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)]
    = s => fa(s).thenCombine(fb(s), toJBiFunction((a: A, b: B) => (a, b)))

  def map[A,B](fa: Par[A])(f: A => B): Par[B]
    = s => fa(s).thenApply(toJFunction(f))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    fork(map(product(a, b))((x) => f(x._1, x._2)))

  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else if (as.size == 1) Par.unit(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  private def toJBiFunction[A, B, C](f: (A, B) => C) =
    new BiFunction[A, B, C] {
      def apply(a: A, b: B) = f(a, b)
    }

  private def toJFunction[A, B](f: A => B) =
    new java.util.function.Function[A, B] {
      def apply(a: A) = f(a)
    }

}




