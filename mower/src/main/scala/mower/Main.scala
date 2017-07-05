package mower

import java.io._
import scala.io.Source

object Orientation extends Enumeration {
  type Orientation = Value
  val N, E, S, W = Value
}

object Action extends Enumeration {
  type Action = Value
  val A = Value('A')
  val G = Value('G')
  val D = Value('D')
}

import Orientation._
import Action._

case class Mower(posX: Int, posY: Int, orientation: Orientation) {

  def runAction(forbiddenLocations: List[(Int,Int)], dimX: Int, dimY: Int, action: Action): Mower = {

    def turnLeft: Mower = {
      val cid = orientation.id
      Mower(posX, posY, if (cid == 0) Orientation(3) else Orientation(cid - 1))
    }

    def turnRight: Mower = {
      val cid = orientation.id
      Mower(posX, posY, if (cid == 3) Orientation(0) else Orientation(cid + 1))
    }

    def outOfBounds(x: Int, y: Int): Boolean = x < 0 || y < 0 || x > dimX || y > dimY

    def forbidden(x: Int, y: Int): Boolean = forbiddenLocations.contains((x, y))

    def move: Mower = orientation match {
      case N => if (outOfBounds(posX, posY + 1) || forbidden(posX, posY + 1)) this else Mower(posX, posY + 1, orientation)
      case S => if (outOfBounds(posX, posY - 1) || forbidden(posX, posY - 1)) this else Mower(posX, posY - 1, orientation)
      case E => if (outOfBounds(posX + 1, posY) || forbidden(posX + 1, posY)) this else Mower(posX + 1, posY, orientation)
      case W => if (outOfBounds(posX - 1, posY) || forbidden(posX - 1, posY)) this else Mower(posX - 1, posY, orientation)
    }

    action match {
      case G => turnLeft
      case D => turnRight
      case A => move
    }
  }

  override def toString(): String = posX + " " + posY + " " + orientation
}

class LawnProgram(val dimX: Int, val dimY: Int, val mowers: Vector[(Mower, Vector[Action])]) {

  require(dimX >= 0, "Dimension x of lawn cannot be negative")
  require(dimY >= 0, "Dimension y of lawn cannot be negative")
  require(mowers.size == mowers.map{m => (m._1.posX, m._1.posY)}.toSet.size, "Overlapping mower positions are not allowed")
  require(mowers.map{m => m._1.posX}.forall(x => x <= dimX && x >= 0), "All mowers have to be inside the bounds of the lawn")
  require(mowers.map{m => m._1.posY}.forall(y => y <= dimY && y >= 0), "All mowers have to be inside the bounds of the lawn")

  def run: Vector[(Mower, Vector[Action])] = {
    (0 to mowers.size - 1).foldLeft(mowers){(acc, index) => 
      val (done, todo) = acc.splitAt(index)
      val (currentMower, actions) = todo.head
      val remaining = todo.tail
      val forbiddenList = (done ++ remaining).map{m => (m._1.posX, m._1.posY)}.toList
      val modifiedMower = actions.foldLeft(currentMower){(m, action) => m.runAction(forbiddenList, dimX, dimY, action)}
      done ++ ((modifiedMower, Vector()) +: remaining)
    }
  }

}

object Main {

  def main(args: Array[String]): Unit = {
    val lp: LawnProgram = readFile(args(0))
    val result = lp.run
    println(result.map{m => m._1}.mkString(sys.props("line.separator")))
  }

  def readFile(inputFile: String): LawnProgram = {
    val lines = Source.fromFile(new File(inputFile)).getLines.toList.filter{s => ! s.trim.isEmpty}
    makeLawnProgram(lines)
  }

  def readOrientation(c: Char): Orientation = c match {
    case 'N' => N
    case 'E' => E
    case 'S' => S
    case 'W' => W
    case _ => throw new IllegalArgumentException(c + " is not a legal orientation")
  }

  def makeLawnProgram(lines: List[String]): LawnProgram = {
    val Array(dimX, dimY) = lines.head.split("\\s+").map {_.toInt}
    val linePairs = lines.tail.grouped(2)
    val mowers: Vector[(Mower, Vector[Action])] = (for ((pair, i) <- linePairs.zipWithIndex) yield {
      val Array(posX, posY, orientation) = pair.head.split("\\s+")
      val x = posX.toInt
      val y = posY.toInt
      assert(x >= 0 && x <= dimX, "Illegal x position for mower " + (i + 1))
      assert(y >= 0 && y <= dimY, "Illegal y position for mower " + (i + 1))
      val actions: Vector[Action] = pair.last.seq.map{c => Action(c.toInt)}.toVector
      (Mower(x, y, readOrientation(orientation.head)), actions)
    }).toVector
    new LawnProgram(dimX, dimY, mowers)
  }

}
