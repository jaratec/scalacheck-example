package mower

import org.scalacheck._
import org.scalacheck.Prop._

import Orientation._
import Action._

object MoveCheck extends Properties("Move Action") {
  property("An advancing mower retains the same orientation and changes position") =
    Prop.forAll(Gen.oneOf(N, S, E, W)) {
      (o: Orientation) => {
        val m1 = Mower(5,5,o)
        val m2 = m1.runAction(List(),10,10,A)
        m1.orientation == m2.orientation &&
        ((m1.posX == m2.posX && m1.posY != m2.posY) || (m1.posX != m2.posX && m1.posY == m2.posY))
      }
    }

  property("Moving north increases the y position") =
    Prop.forAll(Gen.choose(1,7), Gen.choose(1,7)) {
      (x: Int, y: Int) => {
        val m1 = Mower(x,y,N)
        val m2 = m1.runAction(List(),8,8,A)
        m1.posX == m2.posX &&
        (m1.posY + 1) == m2.posY
      }
    }

  property("Moving south decreases the y position") =
    Prop.forAll(Gen.choose(1,7), Gen.choose(1,7)) {
      (x: Int, y: Int) => {
        val m1 = Mower(x,y,S)
        val m2 = m1.runAction(List(),8,8,A)
        m1.posX == m2.posX &&
        (m1.posY - 1) == m2.posY
      }
    }

  property("Moving east increases the x position") =
    Prop.forAll(Gen.choose(1,7), Gen.choose(1,7)) {
      (x: Int, y: Int) => {
        val m1 = Mower(x,y,E)
        val m2 = m1.runAction(List(),8,8,A)
        (m1.posX + 1) == m2.posX &&
        m1.posY == m2.posY
      }
    }

  property("Moving west decreases the x position") =
    Prop.forAll(Gen.choose(1,7), Gen.choose(1,7)) {
      (x: Int, y: Int) => {
        val m1 = Mower(x,y,W)
        val m2 = m1.runAction(List(),8,8,A)
        (m1.posX - 1) == m2.posX &&
        m1.posY == m2.posY
      }
    }

}

object RotationCheck extends Properties("Rotation Action") {
  property("A rotating mower retains the same position and changes orientation") =
    Prop.forAll(Gen.oneOf(N, S, E, W), Gen.oneOf(G, D)) {
      (o: Orientation, a: Action) => {
        val m1 = Mower(5,5,o)
        val m2 = m1.runAction(List(),10,10,a)
        m1.posX == m2.posX &&
        m2.posY == m2.posY &&
        m1.orientation != m2.orientation
      }
    }

  property("A rotating mower ends up in the same orientation when rotated back") = 
    Prop.forAll(Gen.oneOf(N, S, E, W), Gen.oneOf(G, D)) {
      (o: Orientation, a: Action) => {
        val m1 = Mower(5,5,o)
        val m2 = m1.runAction(List(),10,10,a)
        val aInverse = a match {
          case G => D
          case D => G
        }
        val m3 = m2.runAction(List(),10,10,aInverse)
        m1.orientation != m2.orientation &&
        m3.orientation != m2.orientation &&
        m1.orientation == m3.orientation
      }
    }

  property("Rotating a mower 360° preserves its orientation") =
    Prop.forAll(Gen.oneOf(N, S, E, W), Gen.oneOf(G, D)) {
      (o: Orientation, a: Action) => {
        val m1 = Mower(5,5,o)
        val m2 = m1.runAction(List(),10,10,a)
        val m3 = m2.runAction(List(),10,10,a)
        val m4 = m3.runAction(List(),10,10,a)
        val m5 = m4.runAction(List(),10,10,a)
        List(m1,m2,m3,m4).map(_.orientation).toSet.size == 4 &&
        m1.orientation == m5.orientation
      }
    }
}

object ConstraintsCheck extends Properties("Move Constraints") {
  property("A mower stays in-bounds") =
    Prop.forAll(Gen.choose(0,8), Gen.choose(0,8), Gen.oneOf(N, S, E, W), Gen.containerOf[List,Action](Gen.oneOf(A, G, D))) {
      (x: Int, y: Int, o: Orientation, actions: List[Action]) => {
        val m1 = Mower(x,y,o)
        val m2 = actions.foldLeft(m1){(m,a) => m.runAction(List(),8,8,a)}
        m2.posX >= 0 && m2.posX <= 8 &&
        m2.posY >= 0 && m2.posY <= 8
      }
    }

  val pairGen = for {xi <- Gen.choose(0,8); yi <- Gen.choose(0,8)} yield (xi, yi)
  property("A mower doesn't advance over an obstacle") =
    Prop.forAll(Gen.choose(1,7), Gen.choose(1,7), Gen.oneOf(N, S, E, W), Gen.containerOf[List,Action](Gen.oneOf(A, G, D)), Gen.containerOf[List, (Int,Int)](pairGen)) {
      (x: Int, y: Int, o: Orientation, actions: List[Action], locations: List[(Int,Int)]) => {
        val forbiddenLocations = locations.filter{loc => loc != (x,y)}
        val m1 = Mower(x,y,o)
        val m2 = actions.foldLeft(m1){(m,a) => m.runAction(forbiddenLocations,8,8,a)}
        ! forbiddenLocations.contains((m2.posX, m2.posY))
      }
    }
}

object MowerInteractionCheck extends Properties("Mower Interactions") {

  def samePosition(mowers: List[(Mower, _)]): Boolean = {
    val origSize = mowers.size
    val setSize = mowers.map{m => (m._1.posX, m._1.posY)}.toSet.size
    origSize != setSize
  }

  val mowersGen = for {
    x <- Gen.choose(0,8)
    y <- Gen.choose(0,8)
    o <- Gen.oneOf(N, S, E, W)
    actions <- Gen.containerOf[List,Action](Gen.oneOf(A, G, D))
  } yield (Mower(x,y,o), actions.toVector)

  // indirectly testing the creation of the forbidden location list
  property("Mowers do not end up overlapping in the same position") =
    Prop.forAll(Gen.listOfN(10, mowersGen)) {
      (mowers: List[(Mower, Vector[Action])]) => (! samePosition(mowers)) ==> {
        val prog = new LawnProgram(8,8,mowers.toVector)
        val result = prog.run
        ! samePosition(result.toList)
      }
    }

  property("Same number of mowers after finishing the process") =
    Prop.forAll(Gen.listOfN(10, mowersGen)) {
      (mowers: List[(Mower, Vector[Action])]) => (! samePosition(mowers)) ==> {
        val prog = new LawnProgram(8,8,mowers.toVector)
        val result = prog.run
        result.size == mowers.size
      }
    }

  // A actions are filtered out, so only rotations are executed;
  // as a consequence each mower retains its original position
  property("Mowers stay in the same order after finishing the process") =
    Prop.forAll(Gen.listOfN(10, mowersGen)) {
      (mowers: List[(Mower, Vector[Action])]) => (! samePosition(mowers)) ==> {
        val ms1 = mowers.map{m => (m._1, m._2.filter{a => a != A})}.toVector
        val prog = new LawnProgram(8,8,ms1)
        val ms2 = prog.run
        val pairs = ms1.map{m => m._1}.zip(ms2.map{m => m._1})
        pairs.forall{case (m1,m2) => m1.posX == m2.posX && m1.posY == m2.posY}
      }
    }
}
