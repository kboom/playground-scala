package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class CustomTreeTest extends FlatSpec with Matchers {

  "countLeaves" should "work" in {
    CustomTree.countLeaves(Branch(Branch(Leaf(10), Leaf(100)), Leaf(99))) should be (3)
  }

  "maximum element" should "work" in {
    CustomTree.maximum(Branch(Branch(Leaf(10), Leaf(100)), Leaf(99))) should be (100)
  }

  "depth" should "work" in {
    CustomTree.depth(Branch(Branch(Leaf(10), Leaf(100)), Leaf(99))) should be (3)
  }

}
