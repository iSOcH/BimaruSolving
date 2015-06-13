import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

class BimaruBoard(val ships:Map[Int, Int], val occInRows:Seq[Int], val occInCols:Seq[Int], val state:TreeMap[Pos, Cell], private[this] val wasConcluded:Boolean) {
  // "default" ctr
  def this(ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int], state:TreeMap[Pos, Cell]) = this(ships, occInRows, occInCols, state, false)

  // create board full of unknown fields
  def this(ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int]) = {
    this(ships, occInRows, occInCols,
      TreeMap((for (x<-1 to occInCols.length; y<-1 to occInRows.length) yield {
        Pos(x,y) -> Cell.UNKNOWN
      }).toArray:_*)
    )
  }

  def size: Int = {
    if (occInRows.length != occInCols.length) throw new IllegalArgumentException("field has to be a square")
    occInRows.length
  }

  lazy val rows:Seq[TreeMap[Pos,Cell]] = rows(state)
  lazy val rowCells:Seq[Seq[Cell]] = rows.map(_.values.toSeq)
  lazy val cols:Seq[Map[Pos,Cell]] = cols(state)
  lazy val colCells:Seq[Seq[Cell]] = cols.map(_.values.toSeq)
  def lines(implicit l:LineOrientation): Seq[Map[Pos,Cell]] = l match {
    case Row => rows
    case Col => cols
  }
  def occInLine(idx: Int)(implicit orientation: LineOrientation): Int = orientation match {
    case Row => occInRows(idx)
    case Col => occInCols(idx)
  }
  lazy val findShips: (Map[Int,Int], Set[Pos]) = BimaruBoard.findShips(this)

  /**
   * returns a Seq of possible changes to the board
   * NOTE: this may contain changes that lead to a state that violates the rules!
   */
  lazy val possibleSteps: Seq[Seq[(Pos,Cell)]] = {
    val boardToSearchIn = if (wasConcluded) this else concluded

    val neededShipLengths:Seq[Int] = {
      boardToSearchIn.findShips._1.map{ case (length, amount) => length -> (amount, ships.getOrElse(length,0))}
        .filter{ case (_, (foundAmount, neededAmount)) => neededAmount > foundAmount }
        .toSeq.sortBy(-1 * _._1).map(_._1)
    }

    val shipSettingChanges:Seq[Seq[(Pos,Cell)]] = neededShipLengths.flatMap{ length =>
      Seq(Row, Col).flatMap{implicit orientation =>
        if (length == 1 && orientation == Col) {
          Seq()
        } else {
          boardToSearchIn.lines.zipWithIndex
            .filter{ case (_, lineIdx) => occInLine(lineIdx) >= length}
            .flatMap{ case (lineMap,_) =>
              lineMap.toSeq.sliding(length)
                .filter(sl => sl.forall{ case (_,c) => !c.isWater.getOrElse(false)})
                .filterNot(sl => sl.forall{ case (_,c) => c.isShip.getOrElse(false)})
                .filterNot(_.last._2.isNextOpen.getOrElse(false))
                .filterNot(_.head._2.isPrevOpen.getOrElse(false))
                .filter(_.tail.dropRight(1).forall(x => x._2.isPrevOpen.getOrElse(true) && x._2.isNextOpen.getOrElse(true)))
                .map{ changeList =>
                  if (changeList.size == 1) {
                    Seq((changeList.head._1, Cell.SHIP_ONE))
                  } else {
                    Seq((changeList.head._1, Cell.start)) ++
                      changeList.tail.dropRight(1).map{ case (p:Pos,_) => (p, Cell.knownMiddle)} ++
                      Seq((changeList.last._1, Cell.end))
                  }
                }
            }
        }
      }
    }

    shipSettingChanges
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
          val leftRight = pos.leftAndRight.flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
          val leftRightEqualOrUnknown = leftRight.distinct.size <= 1
          val upDown = pos.upAndDown.flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
          val upDownEqualOrUnknown = upDown.distinct.size <= 1

          leftRightEqualOrUnknown && upDownEqualOrUnknown && leftRight.intersect(upDown).isEmpty
        }
      }
    }

    // Anzahl Schiffe in richtiger Grösse
    lazy val shipsOK = {
      val foundShips = findShips._1
      foundShips.keySet == ships.keySet &&
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
    lazy val shipsOK = findShips._1 == ships

    allKnown && rowsShipsOK && colsShipsOK && shipsOK && rulesSatisfied
  }

  lazy val solutions: Stream[BimaruBoard] = {
    val counter = new AtomicLong()
    val tried = new ConcurrentHashMap[ByteBuffer, Unit]()

    possibleSteps.toStream.flatMap { changes =>
      concluded.updated(changes).solveRec(0, tried, counter)
    }
  }

  lazy val uniqueID: ByteBuffer = ByteBuffer.wrap(state.values.map{ _.isShip match {
    case Some(true) => 2
    case Some(false) => 1
    case None => 0
  }}.grouped(4).map(_.zipWithIndex).map(_.map(x => x._1 << x._2*2).sum.toByte).toArray)
  
  override lazy val toString: String = {
    rowCells.map(_.map( _.toString).mkString("|","|","|")).mkString("\n")
  }

  lazy val concluded: BimaruBoard = {
    if (wasConcluded) this
    else {
      var newState = state

      // if remaining unknown fields in row or col can only be water or ship, fill them
      // do-loop because changing the board might offer further possibilities
      var oldState = newState
      do {
        oldState = newState
        newState = {
          val stateRows = rows(newState)
          val stateCols = cols(newState)
          val shipsInRow = stateRows.map(shipsIn)
          val shipsInCol = stateCols.map(shipsIn)
          def shipsInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => shipsInRow(idx)
            case Col => shipsInCol(idx)
          }
          val waterInRow = stateRows.map(_.count(_._2.isWater.getOrElse(false)))
          val waterInCol = stateCols.map(_.count(_._2.isWater.getOrElse(false)))
          def waterInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => waterInRow(idx)
            case Col => waterInCol(idx)
          }
          val unknownsInRow = stateRows.map(_.count(!_._2.isKnown))
          val unknownsInCol = stateCols.map(_.count(!_._2.isKnown))
          def unknownsInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => unknownsInRow(idx)
            case Col => unknownsInCol(idx)
          }

          // on predef fields where we know the direction, put water/ship around
          newState.filter(_._2.isKnownDirection).foreach { case (p, c) =>
            List(c.isLeftOpen.get, c.isUpOpen.get, c.isRightOpen.get, c.isDownOpen.get)
              .zip(List(p.left, p.up, p.right, p.down))
              .filter { case (isOpen, position) => newState.get(position).exists(!_.isKnown) }
              .foreach { case (isOpen, position) =>
              newState = newState.updated(position, if (isOpen) Cell.SHIP else Cell.WATER)
            }
          }

          newState.map { case (p, c) =>
            if (!c.isKnown) {
              val orientations = List(Row, Col)

              if (orientations exists (implicit o => unknownsInLine(p.lineIdx) == occInLine(p.lineIdx) - shipsInLine(p.lineIdx))) {
                // all unknown in row or col have to be ship
                p -> Cell.SHIP
              } else if (orientations exists (implicit o => unknownsInLine(p.lineIdx) == size - occInLine(p.lineIdx) - waterInLine(p.lineIdx))) {
                // all unknown in row or col have to be water
                p -> Cell.WATER
              } else if (p.diagonals exists (newState.get(_).exists(_.isShip.getOrElse(false)))) {
                // a diagonal is a ship --> this field can only be water
                p -> Cell.WATER
              } else {
                val neighbors = (p.leftAndRight ++ p.upAndDown).map(p => p -> newState.get(p))
                val middleNeighbors = neighbors.filter(_._2.contains(Cell.SHIP_MIDDLE)).map(_._1)
                val neighborOfMiddle = middleNeighbors.flatMap(np => np.notInLine(p.orientationTo(np).get).flatMap(newState.get))
                if (neighborOfMiddle.exists(_.isWater.getOrElse(false))) {
                  p -> Cell.SHIP
                } else if (neighborOfMiddle.exists(_.isShip.getOrElse(false))) {
                  p -> Cell.WATER
                } else {
                  p -> c
                }
              }
            } else {
              // dont change a known field
              p -> c
            }
          }
        }
      } while (!oldState.sameElements(newState))

      new BimaruBoard(ships, occInRows, occInCols, newState, true)
    }
  }

  def updated(pos:Pos, cell:Cell): BimaruBoard = updated(List((pos,cell)))

  def updated(changes: Seq[(Pos,Cell)]): BimaruBoard = {
    var newState = state
    for { (pos,cell) <- changes } {
      assert(newState(pos).isWater.map(_ == cell.isWater.get).getOrElse(true),
        s"tried to change water/ship-state of already known $pos, changeset: $changes")

      newState = newState.updated(pos,cell)
    }

    new BimaruBoard(ships, occInRows, occInCols, newState)
  }

  def rows(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.y).toSeq.sortBy(_._1).map(_._2)

  def cols(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.x).toSeq.sortBy(_._1).map(_._2)

  def shipsIn(row: Map[Pos,Cell]): Int = shipsIn(row.values)

  def shipsIn(row: Iterable[Cell]): Int = row.count(_.isShip.getOrElse(false))

  def row(state:TreeMap[Pos,Cell], y:Int): Map[Pos,Cell] = {
    val size = Math.sqrt(state.size).toInt
    for (x<-1 to size) yield {
      val p = Pos(x,y)
      p -> state(p)
    }
  }.toMap

  def col(state:TreeMap[Pos,Cell], x:Int): Map[Pos,Cell] = {
    val size = Math.sqrt(state.size).toInt
    for (y<-1 to size) yield {
      val p = Pos(x,y)
      p -> state(p)
    }
  }.toMap

  private def solveRec(depth: Int, triedStates: ConcurrentHashMap[ByteBuffer, Unit], counter: AtomicLong): Seq[BimaruBoard] = {
    if (counter.incrementAndGet() % 256 == 0) {
      println()
      println(triedStates.size())
      println(this + "\n")
    }

    val alreadyChecked = () equals triedStates.putIfAbsent(uniqueID, ())
    if (alreadyChecked) {
      Seq.empty
    } else {
      if (isSolved) {
        Seq(this)
      } else {
        val parOrStreamSteps = if (depth < BimaruBoard.PARALLEL_RECURSION_DEPTH_LIMIT) {
          possibleSteps.par
        } else possibleSteps.toStream

        /* after we found a change using a 4-field-ship that was valid,
         we should not try to add a 3-field-ship instead, because we will
         need to add the 4-field-ship anyways
         this optimization significantly improves performance (about factor 3) */
        val boards = parOrStreamSteps.map(c => c.length -> this.concluded.updated(c))
        val neededLength = boards.find(_._2.rulesSatisfied).map(_._1)
        for {
          (length, board) <- boards
          if neededLength.contains(length) && board.rulesSatisfied && board.concluded.rulesSatisfied
          solution <- board.concluded.solveRec(depth+1, triedStates, counter)
        } yield {
          solution
        }
      }.seq
    }
  }

  def solveWith(possibilitiesIndex: Int): BimaruBoard = {
    updated(possibleSteps(possibilitiesIndex))
  }

  @tailrec
  final def solveWith(possibilitiesIndices: Int*): BimaruBoard = {
    if (possibilitiesIndices.isEmpty) this
    else solveWith(possibilitiesIndices.head).solveWith(possibilitiesIndices.tail: _*)
  }
}

object BimaruBoard {
  val PARALLEL_RECURSION_DEPTH_LIMIT = 3

  def apply(ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int], state:TreeMap[Pos, Cell]): BimaruBoard = {
    val board = new BimaruBoard(ships, occInRows, occInCols)
    var newState = board.state
    for ((p, c) <- state) {
      newState = newState.updated(p,c)
    }
    new BimaruBoard(ships, occInRows, occInCols, newState)
  }

  def findShips(board:BimaruBoard): (Map[Int,Int], Set[Pos]) = {
    var foundShips:Map[Int,Int] = board.ships.mapValues(_ => 0)
    var usedFields = Set[Pos]()

    Seq(Row, Col).foreach{ implicit orientation =>
      board.lines.foreach { lineMap =>
        var shipLength = 0
        lineMap.withFilter(posCell => !usedFields.contains(posCell._1)).foreach{ case (pos, cell) =>
          if (cell.isShip.getOrElse(false)) {
            val notInLine = pos.notInLine.map(board.state.get)
            if (notInLine.forall(_.map(_.isWater.getOrElse(true)).getOrElse(true))) {
              shipLength += 1

              // if i'm last field in row, count ship!
              if (!board.state.contains(pos.next)) {
                foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
                var usedPos = pos
                for (i <- 0 until shipLength) {
                  usedFields += usedPos
                  usedPos = usedPos.prev
                }
                shipLength = 0
              }
            }
          } else {
            if (shipLength > 0 && cell.isKnown) {
              // we reached the end of a ship
              foundShips = foundShips.updated(shipLength, foundShips.getOrElse(shipLength, 0) + 1)
              var usedPos = pos
              for (i <- 0 until shipLength) {
                usedPos = usedPos.prev
                usedFields += usedPos
              }
              shipLength = 0
            } else if (shipLength > 0) {
              // we dont know yet if the ship ends here, must not count it
              shipLength = 0
            }
          }
        }
      }
    }
    (foundShips, usedFields)
  }
}