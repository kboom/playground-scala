package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomTreeTest extends FlatSpec with Matchers {

  "countLeaves" should "work" in {
    CustomTree.countLeaves(Branch(Branch(Leaf(10), Leaf(100)), Leaf(99))) should be (3)
  }

}
