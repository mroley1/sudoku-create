package soduku
import Array._
import scala.util.control.Breaks
import util.Random




//noinspection RemoveRedundantReturn
class puzzle(){
  var matrix: Array[Array[Int]] = ofDim[Int](9, 9)
  var shadowMatrix: Array[Array[Set[Int]]] = ofDim[Set[Int]](9, 9)
  wipe()


  def printBoard(matrix:Array[Array[Int]]): Unit = {
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
    remaining
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
              val tmp = optionsSquishy(row, i)
              if (tmp.size == 1) {
                matrix(row)(i) = tmp.head
              }
            }
            if (matrix(i)(col) == 0){
              val tmp = optionsSquishy(i, col)
              if (tmp.size == 1) {
                matrix(i)(col) = tmp.head
              }
            }
            val boxRow = (i / 3) + (row / 3) * 3
            val boxCol = (i % 3) + (col / 3) * 3
            if (matrix(boxRow)(boxCol) == 0){
              val tmp = optionsSquishy(boxRow, boxCol)
              if (tmp.size == 1) {
                matrix(boxRow)(boxCol) = tmp.head
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
  def unSolve(trials:Int): Unit ={
    val save:Array[Array[Int]] = matrix.map(_.clone())
    var best:Array[Array[Int]] = ofDim[Int](9, 9)
    var leaderScore:Int = 0
    def count(): Int={
      var enum = 0
      for (row <- 0 to 8; col <- 0 to 8){
        if (matrix(row)(col)==0){
          enum += 1
        }
      }
      enum
    }
    val rand = new Random()
    for (_ <- 0 to trials) {
      for (row <- 0 to 8) {
        matrix(row)(rand.nextInt(9)) = 0
      }
      for (col <- 0 to 8) {
        val row = rand.nextInt(9)
        if (optionsSquishy(row, col).size == 1) {
          matrix(row)(col) = 0
        }
      }
      for (_ <- 0 to 12) {
        val row = rand.nextInt(9)
        val col = rand.nextInt(9)
        if (optionsSquishy(row, col).size == 1) {
          matrix(row)(col) = 0
        }
      }
      for (row <- 0 to 8; col <- 0 to 8) {
        if (matrix(row)(col) != 0) {
          if (optionsSquishy(row, col).size == 1) {
            matrix(row)(col) = 0
          }
        }
      }
      val tally = count()
      if (tally>leaderScore){
        leaderScore = tally
        best = matrix.map(_.clone())
      }
      matrix = save.map(_.clone())
    }
    matrix = best.map(_.clone())
  }
  def solve(): Unit ={
    val rand = new Random()
    var left:Set[(Int, Int)] = Set()
    def replace(coords:(Int, Int)): Boolean ={
      val possible = optionsSquishy(coords._1,coords._2)
      if (possible.size==1){
        matrix(coords._1)(coords._2)=possible.head
        left = left.-(coords)
        return true
      }
      else return false
    }
    for (row <- 0 to 8; col <- 0 to 8){
      if (matrix(row)(col)==0){
        left = left.+((row,col))
      }
    }
    rand.shuffle(left)
    val loop = new Breaks
    loop.breakable{
        for (coords <- left){
          if (replace(coords)){
            for (i <- 0 to 8) {
              if (matrix(coords._1)(i) == 0){
                val tmp = optionsSquishy(coords._1, i)
                if (tmp.size == 1) {
                  matrix(coords._1)(i) = tmp.head
                  left = left.-((coords._1, i))
                }
              }
              if (matrix(i)(coords._2) == 0){
                val tmp = optionsSquishy(i, coords._2)
                if (tmp.size == 1) {
                  matrix(i)(coords._2) = tmp.head
                  left = left.-((i, coords._2))
                }
              }
              val boxRow = (i / 3) + (coords._1 / 3) * 3
              val boxCol = (i % 3) + (coords._2 / 3) * 3
              if (matrix(boxRow)(boxCol) == 0){
                val tmp = optionsSquishy(boxRow, boxCol)
                if (tmp.size == 1) {
                  matrix(boxRow)(boxCol) = tmp.head
                  left = left.-((boxRow, boxCol))
                }
              }
            }
          }
        }
        if (left.isEmpty){loop.break()}
    }
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
      println("Timecheck: " + ((System.currentTimeMillis() - start) * .001))
      println("made into a puzzle:")
      genObj.unSolve(30)
      genObj.printMatrix()
      println("Timecheck: " + ((System.currentTimeMillis() - start) * .001))
      println("attempt at solving:")
      genObj.solve()
      genObj.printMatrix()
    }
    val end = System.currentTimeMillis()
    println("It took " + ((end - start) * .001) + " seconds to generate and solve " + runTimes + " puzzle(s)")
  }
  main()


}
