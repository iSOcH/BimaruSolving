import java.nio.ByteBuffer

object SolverHelper {
  implicit class makeHelped(board: BimaruBoard) {
    lazy val uniqueID: ByteBuffer = ByteBuffer.wrap(board.state.values.map{ _.isShip match {
      case Some(true) => 2
      case Some(false) => 1
      case None => 0
    }}.grouped(4).map(_.zipWithIndex).map(_.map(x => x._1 << x._2*2).sum.toByte).toArray)

    /**
     * returns a Seq of possible changes to the board
     * NOTE: this may contain changes that lead to a state that violates the rules!
     */
    lazy val possibleSteps: Seq[Seq[(Pos,Cell)]] = {
      val neededShipLengths:Seq[Int] = {
        findShips._1.map{ case (length, amount) => length -> (amount, board.ships.getOrElse(length,0))}
          .filter{ case (_, (foundAmount, neededAmount)) => neededAmount >= foundAmount }
          .toSeq.sortBy(-1 * _._1).map(_._1)
      }

      val shipSettingChanges:Seq[Seq[(Pos,Cell)]] = neededShipLengths.flatMap{ length =>
        Seq(Row, Col).flatMap{implicit orientation =>
          if (length == 1 && orientation == Col) {
            Seq() // length 1 is checked in Row orientation
          } else {
            board.lines.zipWithIndex
              .filter{ case (_, lineIdx) => board.occInLine(lineIdx) >= length}
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

    lazy val findShips: (Map[Int,Int], Set[Pos]) = {
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

    lazy val rulesSatisfied: Boolean = {
      // Anzahl Schiffe in Zeilen und Spalten
      val rowsShipsOK = board.shipsInRows.zip(board.occInRows).forall( x => x._1 <= x._2 )
      lazy val colsShipsOK = board.shipsInCols.zip(board.occInCols).forall( x => x._1 <= x._2 )

      // nicht zu viel Wasser in Zeile/Spalte
      val rowsWaterOK = board.rowCells.map(_.count(_.isWater.getOrElse(false))).zip(board.occInRows).forall(x => x._1 <= board.size-x._2)
      lazy val colsWaterOK = board.colCells.map(_.count(_.isWater.getOrElse(false))).zip(board.occInCols).forall(x => x._1 <= board.size-x._2)

      val rowsOK = rowsShipsOK && rowsWaterOK
      lazy val colsOK = colsShipsOK && colsWaterOK

      // freie Felder um Schiffe
      lazy val diagonalSpaceOK = {
        board.state
          .filter(_._2.isShip.getOrElse(false))
          .forall { case (pos, _) =>
          val diagonalCells = pos.diagonals.map(board.state.get).filter(_.isDefined).map(_.get)
          diagonalCells.forall(_.isWater.getOrElse(true))
        }
      }

      lazy val endsOK = {
        board.state.filter(_._2.isPredefinedShip).forall { case (pos, c) =>
          if (c.isKnownDirection) {
            (pos.leftAndRight ++ pos.upAndDown)
              .zip(Seq(c.isLeftOpen,c.isRightOpen,c.isUpOpen,c.isDownOpen).map(_.get))
              .filter{case (p, _) => board.state.get(p).exists(_.isKnown)}
              .forall{case (p, isOpen) => board.state(p).isShip.get == isOpen }
          } else {
            val leftRight = pos.leftAndRight.flatMap(board.state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
            val leftRightEqualOrUnknown = leftRight.distinct.size <= 1
            val upDown = pos.upAndDown.flatMap(board.state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
            val upDownEqualOrUnknown = upDown.distinct.size <= 1

            leftRightEqualOrUnknown && upDownEqualOrUnknown && leftRight.intersect(upDown).isEmpty
          }
        }
      }

      // Anzahl Schiffe in richtiger Grösse
      lazy val shipsOK = {
        val foundShips = findShips._1
        foundShips.keySet == board.ships.keySet &&
          foundShips.forall{ case (length, amount) =>
            board.ships.get(length).exists(_ >= amount)
          }
      }

      rowsOK && colsOK && diagonalSpaceOK && endsOK && shipsOK
    }

    lazy val isSolved: Boolean = {
      // alles muss fix sein
      val allKnown = board.state.forall(_._2.isKnown)

      // Anzahl Schiffe in Zeilen und Spalten
      lazy val rowsShipsOK = board.shipsInRows.zip(board.occInRows).forall( x => x._1 == x._2 )
      lazy val colsShipsOK = board.shipsInCols.zip(board.occInCols).forall( x => x._1 == x._2 )

      // Anzahl Schiffe in richtiger Grösse
      lazy val shipsOK = findShips._1 == board.ships

      allKnown && rowsShipsOK && colsShipsOK && shipsOK && rulesSatisfied
    }

    def updatedAndConclude(pos:Pos, cell:Cell): BimaruBoard = updatedAndConclude(List((pos,cell)))

    def updatedAndConclude(changes: Seq[(Pos,Cell)]): BimaruBoard = {
      var newState = board.state
      for { (pos,cell) <- changes } {
        //assert(newState(pos).isWater.map(_ == cell.isWater.get).getOrElse(true))

        newState = newState.updated(pos,cell)

        // set diagonal fields to water if a ship was added
        if (cell.isShip.get) { // ... it should really be known
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
      }

      // if remaining unknown fields in row or col can only be water or ship, fill them
      // do-loop because changing the board might offer further possibilities
      var oldState = newState
      do {
        oldState = newState
        newState = {
          val stateRows = board.rows(newState)
          val stateCols = board.cols(newState)
          val shipsInRow = stateRows.map(BimaruBoard.shipsIn)
          val shipsInCol = stateCols.map(BimaruBoard.shipsIn)
          def shipsInLine(idx:Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => shipsInRow(idx)
            case Col => shipsInCol(idx)
          }
          val waterInRow = stateRows.map(_.count(_._2.isWater.getOrElse(false)))
          val waterInCol = stateCols.map(_.count(_._2.isWater.getOrElse(false)))
          def waterInLine(idx:Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => waterInRow(idx)
            case Col => waterInCol(idx)
          }
          val unknownsInRow = stateRows.map(_.count(!_._2.isKnown))
          val unknownsInCol = stateCols.map(_.count(!_._2.isKnown))
          def unknownsInLine(idx:Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => unknownsInRow(idx)
            case Col => unknownsInCol(idx)
          }

          newState.map { case (p, c) =>
            if (!c.isKnown) {
              val orientations = List(Row, Col)

              if (orientations exists(implicit o => unknownsInLine(p.lineIdx) == board.occInLine(p.lineIdx) - shipsInLine(p.lineIdx) )) {
                // all unknown in row or col have to be ship
                p -> Cell.SHIP
              } else if (orientations exists(implicit o => unknownsInLine(p.lineIdx) == board.size - board.occInLine(p.lineIdx) - waterInLine(p.lineIdx))) {
                // all unknown in row or col have to be water
                p -> Cell.WATER
              } else if (p.diagonals exists(newState.get(_).exists(_.isShip.getOrElse(false)))) {
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

      new BimaruBoard(board.size, board.ships, board.occInRows, board.occInCols, newState)
    }
  }
}
