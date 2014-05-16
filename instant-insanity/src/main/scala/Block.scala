package com.exceptbuses.blockpuzzle

case class Block(top: Color, bottom: Color, left: Color, right: Color, front: Color, back: Color) {
  override def toString = {
    s"top is $top, bottom is $bottom, left is $left, right is $right, front is $front, back is $back"
  }

  def rotate(axis: Axis): Block = {
    // Always rotate clockwise, because why not?
    axis match {
      case `y` => Block(left = left, right = right, top = front, front = bottom, bottom = back, back = top)
      case `x` => Block(top = left, right = top, bottom = right, left = bottom, front = front, back = back)
      case `z` => Block(top = top, bottom = bottom, left = front, back = left, right = back, front = right)
    }
  }

  def allRotations: Set[Block] = {
    val bx0 = this
    val bx0p1 = bx0.rotate(z)
    val bx0p2 = bx0p1.rotate(z)
    val bx0p3 = bx0p2.rotate(z)

    val bx1 = bx0.rotate(x)
    val bx1p1 = bx1.rotate(y)
    val bx1p2 = bx1p1.rotate(y)
    val bx1p3 = bx1p2.rotate(y)

    val bx2 = bx1.rotate(x)
    val bx2p1 = bx2.rotate(z)
    val bx2p2 = bx2p1.rotate(z)
    val bx2p3 = bx2p2.rotate(z)

    val bx3 = bx2.rotate(x)
    val bx3p1 = bx3.rotate(y)
    val bx3p2 = bx3p1.rotate(y)
    val bx3p3 = bx3p2.rotate(y)

    val by1 = this.rotate(y)
    val by1p1 = by1.rotate(x)
    val by1p2 = by1p1.rotate(x)
    val by1p3 = by1p2.rotate(x)

    val by3 = by1.rotate(y).rotate(y)
    val by3p1 = by3.rotate(x)
    val by3p2 = by3p1.rotate(x)
    val by3p3 = by3p2.rotate(x)

    Set(bx0,
        bx0p1,
        bx0p2,
        bx0p3,
        bx1,
        bx1p1,
        bx1p2,
        bx1p3,
        bx2,
        bx2p1,
        bx2p2,
        bx2p3,
        bx3,
        bx3p1,
        bx3p2,
        bx3p3,
        by1,
        by1p1,
        by1p2,
        by1p3,
        by3,
        by3p1,
        by3p2,
        by3p3        
      )
  }
}

sealed abstract class Color
case object Red extends Color
case object Yellow extends Color
case object Teal extends Color
case object Green extends Color

sealed abstract class Axis
case object y extends Axis
case object x extends Axis
case object z extends Axis

