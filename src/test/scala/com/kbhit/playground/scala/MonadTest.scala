package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class MonadTest extends FlatSpec with Matchers {

  "abe cadlo monoid" should "be abecadlo" in {
    for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b should be ("Hello, monad!")
  }

}
