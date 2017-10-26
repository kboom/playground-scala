package com.kbhit.playground.scala

import org.scalatest.{FlatSpec, _}

class MonoidTest extends FlatSpec with Matchers {

  "abe cadlo monoid" should "be abecadlo" in {
    List("abe", "cadlo").foldRight(Monoid.stringMonoid.zero)(Monoid.stringMonoid.op) should be ("abecadlo")
  }

  "abe cadlod monoid" should "be abecadlo" in {
    List("abe", "cadlo ", " z ", "pieca ").foldRight(Monoid.wordsMonoid.zero)(Monoid.wordsMonoid.op) should be ("abe cadlo z pieca")
  }

}
