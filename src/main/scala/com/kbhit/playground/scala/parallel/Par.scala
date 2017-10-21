package com.kbhit.playground.scala.parallel

import java.util.concurrent.{CompletableFuture, ExecutorService}
import java.util.function.BiFunction

import com.kbhit.playground.scala.parallel.Parallel._

package object Parallel {
  type Par[A] = ExecutorService => CompletableFuture[A]
}

object Par {

  def unit[A](a: A): Par[A] = (_) => CompletableFuture.completedFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (s) => a(s).thenCombine(b(s), toJBiFunction(f))

  def fork[A](a: => Par[A]): Par[A] =
    (s) => a(s)

  def async[A](a: => A): Par[A] = fork(unit(a))

  private def toJBiFunction[A, B, C](predicate: (A, B) => C) =
    new BiFunction[A, B, C] {
      def apply(a: A, b: B) = predicate(a, b)
    }

}




