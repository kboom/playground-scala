package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomStreamTest extends FlatSpec with Matchers {

  "stream of 1,2,3" should "be converted to list [1,2,3]" in {
    CustomStreamImpl(1, 2, 3).toList should be (List(1, 2, 3))
  }

}
