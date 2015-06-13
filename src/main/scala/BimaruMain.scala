import com.triptheone.joda.Stopwatch

import scala.collection.immutable.TreeMap

object BimaruMain extends App {
  val outerWatch = Stopwatch.start()
  println("Searching first Solutions for Boards")
  println("====================================")
  val bims = List(Bimarus.bim6, Bimarus.bim8_9, Bimarus.bim8_13, Bimarus.bim8_13_multipleSolutions,
    Bimarus.bim8_16, Bimarus.bim10_3, Bimarus.bim10_9, Bimarus.bim10_15, Bimarus.bim10_16,
    Bimarus.bim10_conceptis_hard, Bimarus.bim9_blick_150428)
  //val bims = List(bim8_9)

  bims.foreach{ bim =>
    println(s"Starting search for first Solution in\n$bim\n")
    val watch = Stopwatch.start()
    bim.solutions
    println(s"Found first Solution after ${watch.getElapsedTime}\n${bim.solutions.head}\n")
  }
  println(s"Found first Solution for all Boards after ${outerWatch.getElapsedTime}\n\n\n")

  println("Searching remaining Solutions for Boards")
  println("========================================")
  bims.foreach { bim =>
    println(s"Starting search for remaining Solutions in\n$bim\n")
    val watch = Stopwatch.start()
    bim.solutions.force
    println(s"Found remaining Solutions after ${watch.getElapsedTime} - ${bim.solutions.tail.size} additional Solutions found")
    if (bim.solutions.nonEmpty) {
      val linesOfSolutions = bim.solutions.map(_.toString.lines.toIndexedSeq)
      for (lineNr <- linesOfSolutions.head.indices) {
        linesOfSolutions.foreach(lines => print(lines(lineNr) + "   "))
        println()
      }
    }
    println()
  }
  println(s"Found all Solutions for all Boards after ${outerWatch.getElapsedTime}\n\n\n")
}

object Bimarus {

  val bim6 = BimaruBoard(Map(1->3,2->2,3->1), List(2,1,1,4,0,2), List(2,1,2,1,1,3), TreeMap(
    Pos(5,1) -> Cell.SHIP_START_RIGHT,
    Pos(3,2) -> Cell.WATER
  ))

  val bim8_9 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(3,2,0,4,0,5,0,6), List(2,3,2,3,3,3,2,2), TreeMap(
    Pos(4,1) -> Cell.SHIP_ONE,
    Pos(7,2) -> Cell.SHIP_START_RIGHT
  ))

  val bim8_13 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(5,0,3,0,3,4,1,4), List(3,1,2,3,1,4,2,4), TreeMap(
    Pos(1,5) -> Cell.SHIP_ONE,
    Pos(6,6) -> Cell.SHIP_MIDDLE,
    Pos(1,8) -> Cell.WATER
  ))

  val bim8_16 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(3,3,3,1,2,2,3,3), List(6,2,1,3,1,3,2,2), TreeMap(
    Pos(6,1) -> Cell.WATER,
    Pos(8,1) -> Cell.SHIP_START_DOWN)
  )

  val bim10_3 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(2,2,2,3,1,3,2,4,0,1), List(4,1,3,2,1,3,2,1,1,2), TreeMap(
    Pos(3,1) -> Cell.SHIP_START_RIGHT,
    Pos(10,2) -> Cell.WATER,
    Pos(7,3) -> Cell.SHIP_START_DOWN,
    Pos(3,10) -> Cell.SHIP_ONE)
  )

  val bim10_9 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(1,0,1,1,2,3,3,4,0,5), List(1,2,1,4,1,2,0,5,1,3), TreeMap(
    Pos(6,3) -> Cell.WATER,
    Pos(2,6) -> Cell.SHIP_START_DOWN,
    Pos(5,8) -> Cell.SHIP_START_LEFT,
    Pos(6,10) -> Cell.WATER
  ))

  val bim10_15 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(5,0,5,1,3,0,1,0,1,4), List(4,0,4,2,4,3,0,1,1,1), TreeMap(
    Pos(1,3) -> Cell.WATER,
    Pos(9,3) -> Cell.SHIP_START_RIGHT,
    Pos(8,7) -> Cell.SHIP_ONE
  ))

  val bim10_16 = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(4,1,2,2,0,1,1,2,4,3), List(1,2,1,2,4,2,1,3,0,4), TreeMap(
    Pos(2,3) -> Cell.SHIP_START_DOWN,
    Pos(3,6) -> Cell.SHIP_ONE,
    Pos(10,8) -> Cell.SHIP_START_UP
  ))

  val bim8_13_multipleSolutions = bim8_13.updated( Pos(6,6), Cell.SHIP )

  // http://www.conceptispuzzles.com/de/index.aspx?uri=puzzle/euid/01000000edbb3ae5249879c6e9cd4e16ba77208c40a0f0b59d53c404a6ef5301e6cf2d212c86934e8868eccb3467dbf784b3a7d4/play
  val bim10_conceptis_hard = BimaruBoard(Map(1->4,2->3,3->2,4->1), List(2,3,2,3,1,1,3,2,2,1), List(1,1,4,0,3,0,3,1,3,4), TreeMap(
    Pos(9,1) -> Cell.SHIP_START_RIGHT,
    Pos(5,9) -> Cell.SHIP_ONE,
    Pos(10,9) -> Cell.SHIP_MIDDLE
  ))

  val bim9_blick_150428 = BimaruBoard(Map(1->4, 2->3, 3->2, 4->1), List(3,0,2,2,2,6,1,1,3), List(3,1,1,2,2,3,4,0,4), TreeMap(
    Pos(6,1) -> Cell.WATER,
    Pos(3,4) -> Cell.SHIP_START_RIGHT,
    Pos(1,8) -> Cell.SHIP_ONE
  ))
}