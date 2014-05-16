package com.exceptbuses.blockpuzzle

import org.scalatest._

class TestPuzzle extends FlatSpec with Matchers {
  val b1 = Block(top = Yellow, left = Teal, right = Red, front = Green, back = Yellow, bottom = Yellow)
  "A block" should "rotate itself in the x dimension"  in {
    b1.rotate(x) should equal (Block(top = Teal, right = Yellow, left = Yellow, front = Green, back = Yellow, bottom = Red))
  }
  "A block" should "rotate itself in the y dimension"  in {
    b1.rotate(y) should equal (Block(top = Green, left = Teal, right = Red, front = Yellow, back = Yellow, bottom = Yellow))
  }
  "A block" should "rotate itself in the z dimension"  in {
    b1.rotate(z) should equal (Block(top = Yellow, left = Green, right = Yellow, front = Red, back = Teal, bottom = Yellow))
  }
  "A block" should "return to the identity after 4 rotations in any direction"  in {
    b1.rotate(x).rotate(x).rotate(x).rotate(x) should equal (b1)
    b1.rotate(y).rotate(y).rotate(y).rotate(y) should equal (b1)
    b1.rotate(z).rotate(z).rotate(z).rotate(z) should equal (b1)
  }
  "The allRotations method" should "return a set of 24 elements for for this given block" in {
    val rotations = b1.allRotations
    rotations.size should equal (24)
  }

  "The allRotations method" should "return a set of 6 elements for for this a block with only one different side" in {
    val b2 = Block(top = Yellow, left = Teal, right = Yellow, front = Yellow, back = Yellow, bottom = Yellow)
    val rotations = b2.allRotations
    rotations.size should equal (6)
  }
}

