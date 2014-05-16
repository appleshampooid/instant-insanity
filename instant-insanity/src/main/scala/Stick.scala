package com.exceptbuses.blockpuzzle

class Stick(val blocks: List[Block]) {
  require(blocks.size > 0, "Blocks must not be an empty list")

  def invalidSides = {
    val allColors: Set[Color] = Set(Red, Yellow, Green, Teal)
    List(allColors &~ blocks.map(_.left).toSet,
         allColors &~ blocks.map(_.right).toSet,
         allColors &~ blocks.map(_.top).toSet,
         allColors &~ blocks.map(_.bottom).toSet)
  }

  def valid = {
    blocks.map(_.left).toSet.size == 4 &&  
    blocks.map(_.right).toSet.size == 4 && 
    blocks.map(_.top).toSet.size == 4 &&   
    blocks.map(_.bottom).toSet.size == 4
  }

  override def toString = {
    blocks.map(_.toString + "\n").mkString
  }
}

object Stick {
  def main(args: Array[String]) = {
    val b1 = Block(top = Red, left = Red, right = Yellow, front = Teal, back = Yellow, bottom = Green)
    val b2 = Block(top = Yellow, left = Teal, right = Red, front = Green, back = Yellow, bottom = Yellow)
    val b3 = Block(top = Teal, left = Green, right = Green, front = Yellow, back = Teal, bottom = Red)
    val b4 = Block(top = Green, left = Green, right = Red, front = Red, back = Teal, bottom = Yellow)
    
    val blocks = List(b1, b2, b3, b4)
    val stick = new Stick(blocks)

    val sticks: Set[Stick] = 
      for (b1r <- b1.allRotations;
           b2r <- b2.allRotations;
           b3r <- b3.allRotations;
           b4r <- b4.allRotations) 
      yield new Stick(List(b1r, b2r, b3r, b4r))
    println(sticks.filter(_.valid).map(_.toString + "\n").mkString)
  }
}
