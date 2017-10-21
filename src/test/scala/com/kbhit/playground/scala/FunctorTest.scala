package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class FunctorTest extends FlatSpec with Matchers {

  "distribute" should "work" in {
    listFunctor.distribute(List((1,2), (3,4))) should be ((List(1,3), List(2, 4)))
  }

}
