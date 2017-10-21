package com.kbhit.playground.scala.parallel

import java.util.concurrent.Executors

import org.scalatest.{FlatSpec, _}

class ParTest extends FlatSpec with Matchers {

  private val threadPool = Executors.newFixedThreadPool(2)

  "map two units 1, 3, +" should "be 4" in {
    Par.map2(Par.unit(1), Par.unit(3))(_ + _)(threadPool).get() should be (4)
  }

  "map two async 4, 2, +" should "be 6" in {
    Par.map2(Par.async(4), Par.async(2))(_ + _)(threadPool).get() should be (6)
  }

  "parallel sum of [1,2,3,4,5,6,7,8,9,10]" should "be 6" in {
    Par.sum(1 to 10)(threadPool).get() should be (55)
  }

}
