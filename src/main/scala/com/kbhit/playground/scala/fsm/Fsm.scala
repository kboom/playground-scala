package com.kbhit.playground.scala.fsm

import scalaz.{State, Scalaz}, Scalaz._

object Fsm {
  def apply[I, S](f: PartialFunction[(I, S), S]): Fsm[I, S] =
    new Fsm((i, s) => f.applyOrElse((i, s), (_: (I, S)) => s))

  private def states[S, O](xs: List[State[S, O]]): State[S, List[O]] =
    xs.sequence[({type λ[α]=State[S, α]})#λ, O]

  private def modify[I, S](f: (I, S) => S): I => State[S, Unit] =
    i => State.modify[S](s => f(i, s))
}

final class Fsm[I, S] private (f: (I, S) => S) {
  def apply(is: List[I]): State[S, S] =
    Fsm.states(is.map(Fsm.modify(f))).flatMap(_ => State.get[S])

  def run(is: List[I]): State[S, S] =
    apply(is)
}
