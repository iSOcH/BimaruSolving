import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import scala.collection.immutable.TreeMap

/**
 * Created by iso on 21.01.15.
 */

object BimaruBoard {
  def apply(size:Int, ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int], state:TreeMap[Pos, Cell]): BimaruBoard = {
    var board = new BimaruBoard(size, ships, occInRows, occInCols)
    for ((p,c) <- state) {
      board = board.updated(p,c)
    }
    board
  }

  def findShips(board:BimaruBoard): (Map[Int,Int], Set[Pos]) = {
    var foundShips:Map[Int,Int] = board.ships.mapValues(_ => 0)
    var usedFields = Set[Pos]()

    board.rows.foreach{ rowMap =>
      var shipLength = 0
      rowMap.foreach { case (pos, cell) =>
        val Pos(x,y) = pos
        if (cell.isShip.getOrElse(false)) {
          val upAndDown = pos.upAndDown.map(board.state.get)
          if (upAndDown.forall(_.map(_.isWater.getOrElse(true)).getOrElse(true))) {
            shipLength += 1

            // if i'm last field in row, count ship!
            if (!board.state.contains(pos.right)) {
              foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
              while (shipLength > 0) {
                shipLength -= 1
                usedFields += Pos(x-shipLength, y)
              }
            }
          }
        } else {
          if (shipLength > 0 && cell.isKnown) {
            // we reached the end of a ship
            foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
            while (shipLength > 0) {
              usedFields += Pos(x-shipLength, y)
              shipLength -= 1
            }
          } else if (shipLength > 0) {
            // we dont know yet if the ship ends here, must not count it
            shipLength = 0
          }
        }
      }
    }

    board.cols.foreach{ colMap =>
      var shipLength = 0
      colMap.filterKeys(!usedFields.contains(_)).foreach { case (pos, cell) =>
        val Pos(x,y) = pos
        if (cell.isShip.getOrElse(false)) {
          val leftAndRight = pos.leftAndRight.map(board.state.get)
          if (leftAndRight.forall(_.map(_.isWater.getOrElse(true)).getOrElse(true))) {
            shipLength += 1

            // if i'm last field in row, count ship!
            if (!board.state.contains(pos.down)) {
              foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
              while (shipLength > 0) {
                shipLength -= 1
                usedFields += Pos(x, y-shipLength)
              }
            }
          }
        } else {
          if (shipLength > 0 && cell.isKnown) {
            // we reached the end of a ship
            foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
            while (shipLength > 0) {
              shipLength -= 1
              usedFields += Pos(x,y-shipLength)
            }
          } else if (shipLength > 0) {
            // we dont know yet if the ship ends here, must not count it
            shipLength = 0
          }
        }
      }
    }
    (foundShips, usedFields)
  }
}

class BimaruBoard(val size:Int, val ships:Map[Int, Int], val occInRows:Seq[Int], val occInCols:Seq[Int], val state:TreeMap[Pos, Cell]) {
  // create board full of unknown fields
  def this(size:Int, ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int]) = {
    this(size, ships, occInRows, occInCols,
      TreeMap((for (x<-1 to size; y<-1 to size) yield {
          Pos(x,y) -> Cell.UNKNOWN
      }).toArray:_*)
    )
  }

  def updated(pos:Pos, cell:Cell): BimaruBoard = {
    var newState = state.updated(pos,cell)

    /*
     optimize
    */
    // set diagonal fields to water if a ship was added
    if (cell.isShip.get) {
      val diagonalPos = pos.diagonals.filter(newState.contains)
      for (dP <- diagonalPos) {
        if (!newState(dP).isKnown) {
          newState = newState.updated(dP, Cell.WATER)
        }
      }
    }

    // on predef fields where we know the direction, put water/ship around
    newState.filter(_._2.isKnownDirection).foreach{ case (p, c) =>
      List(c.isLeftOpen.get, c.isUpOpen.get, c.isRightOpen.get, c.isDownOpen.get)
        .zip( List(p.left, p.up, p.right, p.down) )
        .filter{ case (isOpen, position) => newState.get(position).exists(!_.isKnown) }
        .foreach{ case (isOpen, position) =>
          newState = newState.updated(position, if (isOpen) Cell.SHIP else Cell.WATER)
      }
    }

    // if remaining unknown fields in row or col can only be water or ship, fill them
    // do-loop because changing the board might offer further possibilities
    var oldState = newState
    do {
      oldState = newState
      newState = {
        val stateRows = rows(newState)
        val stateCols = cols(newState)
        val shipsInRow = stateRows.map(shipsIn(_))
        val shipsInCol = stateCols.map(shipsIn(_))
        val waterInRow = stateRows.map(_.count(_._2.isWater.getOrElse(false)))
        val waterInCol = stateCols.map(_.count(_._2.isWater.getOrElse(false)))
        val unknownsInRow = stateRows.map(_.count(!_._2.isKnown))
        val unknownsInCol = stateCols.map(_.count(!_._2.isKnown))

        newState.map { case (p, c) =>
          if (!c.isKnown) {
            val Pos(x, y) = p
            if (unknownsInRow(y - 1) == occInRows(y - 1) - shipsInRow(y - 1) ||
              unknownsInCol(x - 1) == occInCols(x - 1) - shipsInCol(x - 1)) {
              // all unknown in row or col have to be ship
              p -> Cell.SHIP
            } else if (unknownsInRow(y - 1) == size - occInRows(y - 1) - waterInRow(y - 1) ||
              unknownsInCol(x - 1) == size - occInCols(x - 1) - waterInCol(x - 1)) {
              // all unknown in row or col have to be water
              p -> Cell.WATER
            } else if (p.diagonals.exists(newState.get(_).exists(_.isShip.getOrElse(false)))) {
              // a diagonal is a ship --> this field can only be water
              p -> Cell.WATER
            } else if ((p.leftAndRight ++ p.upAndDown).exists(newState.get(_).contains(Cell.SHIP_MIDDLE))) {
              val (mp, _) = (p.leftAndRight ++ p.upAndDown).map(p => p -> newState.get(p))
                .filter{ case (_,middlecell) => middlecell.contains(Cell.SHIP_MIDDLE) }.head
              if (p.leftAndRight.contains(mp)) {
                if (mp.upAndDown.exists(newState.get(_).map(_ == Cell.WATER).getOrElse(true))) {
                  // we are left or right of SHIP_MIDDLE and above or under it is water or end of board
                  p -> Cell.SHIP
                } else if (mp.upAndDown.exists(newState.get(_).exists(_.isShip.getOrElse(false)))) {
                  // above or under SHIP_MIDDLE is a ship (NOT end of field!)
                  p -> Cell.WATER
                } else {
                  p -> c
                }
              } else if (p.upAndDown.contains(mp)) {
                if (mp.leftAndRight.exists(newState.get(_).map(_ == Cell.WATER).getOrElse(true))) {
                  // we are above or under SHIP_MIDDLE and left or right of it is water or end of board
                  p -> Cell.SHIP
                } else if (mp.leftAndRight.exists(newState.get(_).exists(_.isShip.getOrElse(false)))) {
                  p -> Cell.WATER
                } else {
                  p -> c
                }
              } else {
                p -> c
              }
            } else {
              // not enough information
              p -> c
            }
          } else {
            // dont change a known field
            p -> c
          }
        }
      }
    } while (!oldState.sameElements(newState))

    new BimaruBoard(size, ships, occInRows, occInCols, newState)
  }

  def row(state:TreeMap[Pos,Cell], y:Int): Map[Pos,Cell] = {
    val size = Math.sqrt(state.size).toInt
    for (x<-1 to size) yield {
      val p = Pos(x,y)
      p -> state(p)
    }
  }.toMap
  def rows(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.y).toSeq.sortBy(_._1).map(_._2)
  lazy val rows:Seq[TreeMap[Pos,Cell]] = rows(state)
  lazy val rowCells:Seq[Seq[Cell]] = rows.map(_.values.toSeq)

  def col(state:TreeMap[Pos,Cell], x:Int): Map[Pos,Cell] = {
    val size = Math.sqrt(state.size).toInt
    for (y<-1 to size) yield {
      val p = Pos(x,y)
      p -> state(p)
    }
  }.toMap
  def cols(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.x).toSeq.sortBy(_._1).map(_._2)
  lazy val cols:Seq[Map[Pos,Cell]] = cols(state)
  lazy val colCells:Seq[Seq[Cell]] = cols.map(_.values.toSeq)

  lazy val findShips: (Map[Int,Int], Set[Pos]) = BimaruBoard.findShips(this)

  lazy val possibleSteps: Seq[(Pos,Cell)] = {
    // try only ship-adding changes as long as possible (those allow way more implications in 'updated')
    // in fact, after placing the last ship 'updated' will fill the rest of the board with water, so
    // this method here doesn't even really need to return water-adding changes
    val shipOrWater = if (occInRows.sum > shipsInRows.sum) Cell.SHIP else Cell.WATER
    state.filterNot(_._2.isKnown).toSeq.map { case (p, _) =>
      p -> shipOrWater
    }.sortWith { (p1, p2) =>
      // try cells in rows and columns that may contain larger ships first
      val occForP1 = Math.max(occInRows(p1._1.y-1), occInCols(p1._1.x-1))
      val occForP2 = Math.max(occInRows(p2._1.y-1), occInCols(p2._1.x-1))
      occForP1 > occForP2
    }
  }

  def shipsIn(row: Map[Pos,Cell]): Int = shipsIn(row.values)

  def shipsIn(row: Iterable[Cell]): Int = {
    row.count(_.isShip.getOrElse(false))
  }

  lazy val shipsInRows: Seq[Int] = {
    rowCells.map(shipsIn(_))
  }

  lazy val shipsInCols: Seq[Int] = {
    colCells.map(shipsIn(_))
  }

  lazy val rulesSatisfied: Boolean = {
    // Anzahl Schiffe in Zeilen und Spalten
    val rowsShipsOK = shipsInRows.zip(occInRows).forall( x => x._1 <= x._2 )
    lazy val colsShipsOK = shipsInCols.zip(occInCols).forall( x => x._1 <= x._2 )

    // nicht zu viel Wasser in Zeile/Spalte
    val rowsWaterOK = rowCells.map(_.count(_.isWater.getOrElse(false))).zip(occInRows).forall(x => x._1 <= size-x._2)
    lazy val colsWaterOK = colCells.map(_.count(_.isWater.getOrElse(false))).zip(occInCols).forall(x => x._1 <= size-x._2)

    val rowsOK = rowsShipsOK && rowsWaterOK
    lazy val colsOK = colsShipsOK && colsWaterOK

    // freie Felder um Schiffe
    lazy val diagonalSpaceOK = {
      state
        .filter(_._2.isShip.getOrElse(false))
        .forall { case (pos, _) =>
          val diagonalCells = pos.diagonals.map(state.get).filter(_.isDefined).map(_.get)
          diagonalCells.forall(_.isWater.getOrElse(true))
      }
    }

    lazy val endsOK = {
      state.filter(_._2.isPredefinedShip).forall { case (pos, c) =>
        if (c.isKnownDirection) {
          (pos.leftAndRight ++ pos.upAndDown)
            .zip(Seq(c.isLeftOpen,c.isRightOpen,c.isUpOpen,c.isDownOpen).map(_.get))
            .filter{case (p, _) => state.get(p).exists(_.isKnown)}
            .forall{case (p, isOpen) =>
              state(p).isShip.get == isOpen
          }
        } else {
          val leftRight = pos.leftAndRight.flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get).toSeq
          val leftRightEqualOrUnknown = leftRight.distinct.size <= 1
          val upDown = pos.upAndDown.flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get).toSeq
          val upDownEqualOrUnknown = upDown.distinct.size <= 1

          leftRightEqualOrUnknown && upDownEqualOrUnknown && leftRight.intersect(upDown).size == 0
        }
      }
    }

    // Anzahl Schiffe in richtiger Grösse
    lazy val shipsOK = {
      val foundShips = findShips._1
      foundShips.keySet.sameElements(ships.keySet) &&
      foundShips.forall{ case (length, amount) =>
        ships.get(length).exists(_ >= amount)
      }
    }

    rowsOK && colsOK && diagonalSpaceOK && endsOK && shipsOK
  }

  lazy val isSolved: Boolean = {
    // alles muss fix sein
    val allKnown = state.forall(_._2.isKnown)

    // Anzahl Schiffe in Zeilen und Spalten
    lazy val rowsShipsOK = shipsInRows.zip(occInRows).forall( x => x._1 == x._2 )
    lazy val colsShipsOK = shipsInCols.zip(occInCols).forall( x => x._1 == x._2 )

    // Anzahl Schiffe in richtiger Grösse
    lazy val shipsOK = findShips._1.sameElements(ships)

    allKnown && rowsShipsOK && colsShipsOK && shipsOK && rulesSatisfied
  }

  lazy val solutions: Stream[BimaruBoard] = {
    val counter =  new AtomicLong()
    val tried =  new ConcurrentHashMap[String, Unit]()

    possibleSteps.toStream.flatMap { case (p, c) =>
      updated(p,c).solveRec(parallel=true, tried, counter)
    }
  }

  private def solveRec(parallel:Boolean, triedStates:ConcurrentHashMap[String, Unit], counter:AtomicLong): Seq[BimaruBoard] = {
    if (counter.incrementAndGet() % 15000 == 0) {
      println()
      println(triedStates.size())
      println(this + "\n")
    }

    if (isSolved) {
      Seq(this)
    } else {
      val parOrSeqSteps = if (parallel) possibleSteps.par else possibleSteps.seq
      parOrSeqSteps.flatMap { case (p, c) =>
        val newBoard = updated(p, c)
        val alreadyChecked = () equals triedStates.putIfAbsent(newBoard.uniqueID, ())
        if (!alreadyChecked && newBoard.rulesSatisfied) {
          newBoard.solveRec(parallel=false, triedStates, counter)
        } else {
          Seq.empty
        }
      }.seq
    }
  }

  lazy val uniqueID: String = {
    state.flatMap(_._2.toString).mkString
  }

  override lazy val toString: String = {
    rowCells.map(_.map( _.toString).mkString("|","|","|")).mkString("\n")
  }
}