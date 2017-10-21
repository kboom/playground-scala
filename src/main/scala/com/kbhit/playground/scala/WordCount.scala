package com.kbhit.playground.scala

sealed trait WordCount
case class Stub(chars: String) extends WordCount
case class Part(lStub: String, words: Int, rStub: String) extends WordCount

object WordCountMonoid extends Monoid[WordCount] {

  override def op(a1: WordCount, a2: WordCount) = (a1, a2) match {
    case (Stub(c), Stub(d)) => Stub(c + d)
    case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
    case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }

  override def zero = Stub("")

}