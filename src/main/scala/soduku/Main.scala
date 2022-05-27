package soduku
import Array._
import scala.util.control.Breaks
import util.Random

//noinspection RemoveRedundantReturn
class puzzle(){
  var matrix: Array[Array[Int]] = ofDim[Int](9, 9)
  var shadowMatrix: Array[Array[Set[Int]]] = ofDim[Set[Int]](9, 9)
  wipe()
  def wipe(): Unit = {
    for (r <- 0 to 8) {
      for (c <- 0 to 8) {
        matrix(r)(c) = 0
        shadowMatrix(r)(c) = Set()
      }
    }
  }
  def optionsSquishy(row:Int, col:Int): Set[Int] ={
    var remaining: Set[Int] = Set(1,2,3,4,5,6,7,8,9)
    var remove:Set[Int] = Set()
    for (i <- 0 to 8) {
      remove=remove.+(matrix(row)(i))
      remove=remove.+(matrix(i)(col))
      remove=remove.+(matrix((i/3)+(row/3)*3)((i%3)+(col/3)*3))
      remove=remove.-(matrix(row)(col))
    }
    remaining=remaining.&~(remove)
    //TODO improve options calculator
    return remaining
  }
  def updateShadow(): Unit ={
    for (row <- 0 to 8; col <- 0 to 8){
      shadowMatrix(row)(col) = optionsSquishy(row, col)
    }
  }
  def precisionUpdateShadow(row:Int, col:Int): Unit ={
    for (i <- 0 to 8){
      shadowMatrix(row)(i) = optionsSquishy(row, i)
      shadowMatrix(i)(col) = optionsSquishy(i, col)
      shadowMatrix((i/3)+(row/3)*3)((i%3)+(col/3)*3) = optionsSquishy((i/3)+(row/3)*3, (i%3)+(col/3)*3)
    }
  }
  def generate(): Unit ={
    val rand = new Random()
    var clear = false
    while (!clear) {
      clear = true
      wipe()
      val loop = new Breaks
      loop.breakable {
        for (row <- 0 to 8; col <- 0 to 8) {
          if (matrix(row)(col) == 0) {
            val possible = optionsSquishy(row, col).toVector
            if (possible.nonEmpty) {
              matrix(row)(col) = possible(rand.nextInt(possible.size))
            }
            else {
              clear = false
              loop.break()
            }
          }
          for (i <- 0 to 8) {
            if (matrix(row)(i) == 0){
              if (optionsSquishy(row, i).size == 1) {
                matrix(row)(i) = optionsSquishy(row, i).toVector(0)
              }
            }
            if (matrix(i)(col) == 0){
              if (optionsSquishy(i, col).size == 1) {
                matrix(i)(col) = optionsSquishy(i, col).toVector(0)
              }
            }
            val boxRow = (i / 3) + (row / 3) * 3
            val boxCol = (i % 3) + (col / 3) * 3
            if (matrix(boxRow)(boxCol) == 0){
              if (optionsSquishy(boxRow, boxCol).size == 1) {
                matrix(boxRow)(boxCol) = optionsSquishy(boxRow, boxCol).toVector(0)
              }
            }
          }
        }
      }
    }
  }
  def printMatrix(): Unit ={
    for (i <- 0 to 8) {
      if (i%3==0){println(" +-------+-------+-------+")}
      for ( j <- 0 to 8) {
        if (j%3==0){print(" | ")}
        else print(" ")
        if (matrix(i)(j)!=0){print(matrix(i)(j))}
        else {print(" ")}
      }
      println(" | ")
    }
    println(" +-------+-------+-------+")
  }
}

object Main extends App{
  def main(): Unit = {
    val genObj = new puzzle
    val runTimes = 1
    val start = System.currentTimeMillis()
    for (_ <- 0 until runTimes){
      genObj.generate()
      genObj.printMatrix()
    }
    val end = System.currentTimeMillis()
    print("It took " + ((end - start) * .001) + " seconds to generate " + runTimes + " puzzle(s)")

  }
  main()
}
