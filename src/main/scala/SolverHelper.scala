import java.nio.ByteBuffer

trait SolverHelper extends BimaruBoard {

  lazy val concluded: BimaruBoard with SolverHelper = {
    if (!rulesSatisfied) this
    else {

      var newState = state

      // if remaining unknown fields in row or col can only be water or ship, fill them
      // do-loop because changing the board might offer further possibilities
      var oldState = newState
      do {
        oldState = newState
        newState = {
          val stateRows = BimaruBoard.rows(newState)
          val stateCols = BimaruBoard.cols(newState)
          val shipsInRow = stateRows.map(BimaruBoard.shipsIn)
          val shipsInCol = stateCols.map(BimaruBoard.shipsIn)
          def shipsInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => shipsInRow(idx)
            case Col => shipsInCol(idx)
          }
          val waterInRow = stateRows.map(_.count(_._2 == Water))
          val waterInCol = stateCols.map(_.count(_._2 == Water))
          def waterInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => waterInRow(idx)
            case Col => waterInCol(idx)
          }
          val unknownsInRow = stateRows.map(_.count(_._2 == Unknown))
          val unknownsInCol = stateCols.map(_.count(_._2 == Unknown))
          def unknownsInLine(idx: Int)(implicit orientation: LineOrientation) = orientation match {
            case Row => unknownsInRow(idx)
            case Col => unknownsInCol(idx)
          }

          // on predef fields where we know the direction, put water/ship around
          newState.collect{ case (p,c:Ship) if c.isKnownDirection => (p,c) }.foreach { case (p, c) =>
            List(c.isLeftOpen.get, c.isUpOpen.get, c.isRightOpen.get, c.isDownOpen.get)
              .zip(List(p.left, p.up, p.right, p.down))
              .collect{ case (false, pos) if newState.get(pos).contains(Unknown) => pos }
              .foreach { pos => newState = newState.updated(pos, Water) }
          }

          val orientations = List(Row, Col)
          newState.map { case (p, c) =>
            if (c == Unknown) {
              if (orientations exists (implicit o => unknownsInLine(p.lineIdx) == occInLine(p.lineIdx) - shipsInLine(p.lineIdx))) {
                // all unknown in row or col have to be ship
                p -> Ship.SHIP

              } else if (orientations exists (implicit o => unknownsInLine(p.lineIdx) == size - occInLine(p.lineIdx) - waterInLine(p.lineIdx))) {
                // all unknown in row or col have to be water
                p -> Water

              } else if (p.notDiagonals.collect(newState).collect{ case c:Ship if c.isStartEnd => c }.nonEmpty) {//exists (newState.get(_).exists(_.isStartEnd))) {
                // this would need further checks (lineorientation etc.),
                // but the loop before sets the 'wrong' fields to water already
                p -> Ship.SHIP

              } else if (p.diagonals.collect(newState).exists(_.isShip)) {// exists (newState.get(_).exists(_.isShip.getOrElse(false)))) {
                // a diagonal is a ship --> this field can only be water
                p -> Water

              } else {
                val neighbors = p.notDiagonals.collect{case pos if newState.contains(pos) => pos -> newState(pos)} //.map(p => p -> newState.get(p))
                val middleNeighbors = neighbors.filter(_._2 == Ship.SHIP_MIDDLE).map(_._1)
                val neighborOfMiddle = middleNeighbors.flatMap(np => np.notInLine(p.orientationTo(np).get).collect(newState))
                if (neighborOfMiddle contains Water) {
                  p -> Ship.SHIP
                } else {
                  p -> c
                }
              }
            } else {
              // dont change a known field
              p -> c
            }
          }

          // make board more "clean"
          .map { case (p, c) =>
            if (c == Ship.SHIP_MIDDLE) {
              val newCell = orientations.flatMap(implicit o => {
                if (p.inLine.collect(newState).exists(_.isShip)
                  || p.notInLine.collect(newState).contains(Water)) {
                  Some(Ship.knownMiddle)
                } else None
              }).headOption
              p -> newCell.getOrElse(c)

            } else if (c.isShip) {
              val info = orientations.flatMap { implicit o =>
                if (p.inLine.collect(newState).exists(_.isShip)
                  || p.notInLine.collect(newState).contains(Water)) {
                  // --> part of a ship in current direction
                  val cShip = c.asInstanceOf[Ship]

                  // next has to be ship and this has to be allowed to be < or ^
                  val mayStart = newState.get(p.next).exists(_.isShip) && cShip.isNextOpen.getOrElse(true) && !cShip.isPrevOpen.contains(true)

                  // prev has to be ship and this has to be allowed to be > or v
                  val mayEnd = newState.get(p.prev).exists(_.isShip) && cShip.isPrevOpen.getOrElse(true) && !cShip.isNextOpen.contains(true)

                  // prev is land or water
                  val starts = mayStart && (!newState.contains(p.prev) || newState(p.prev) == Water)

                  // next is land or water
                  val ends = mayEnd && (!newState.contains(p.next) || newState(p.next) == Water)

                  if (starts && ends) {
                    Some(Ship.SHIP_ONE)
                  } else if (starts) {
                    Some(Ship.start)
                  } else if (ends) {
                    Some(Ship.end)
                  } else {
                    None //Some(Cell.knownMiddle)
                  }

                } else None
              }

              if (info.length == 2 && info.forall(_ == Ship.SHIP_ONE)) {
                // TODO: this does not happen enough
                p -> Ship.SHIP_ONE
              } else if (info.length == 1 && info.head != Ship.SHIP_ONE) {
                p -> info.head
              } else p -> c


            } else {
              // cannot make water or unknown "cleaner"
              p -> c
            }
          }
        }
        //println(BimaruBoard.printState(newState) + "\n")
      } while (oldState != newState)

      new BimaruBoard(ships, occInRows, occInCols, newState) with SolverHelperConcluded
    }
  }

  /**
  * returns a Seq of possible changes to the board
  * NOTE: this may contain changes that lead to a state that violates the rules!
  */
  lazy val possibleSteps: Seq[Seq[(Pos,Cell)]] = {
    if (!rulesSatisfied || !concluded.rulesSatisfied) Seq()
    else {
      val neededShipLengths: Seq[Int] = {
        concluded.findShips._1.map { case (length, amount) => length ->(amount, ships.getOrElse(length, 0)) }
          .filter { case (_, (foundAmount, neededAmount)) => neededAmount > foundAmount }
          .toSeq.sortBy(-1 * _._1).map(_._1)
      }

      val shipSettingChanges: Seq[Seq[(Pos, Cell)]] = neededShipLengths.flatMap { length =>
        Seq(Row, Col).flatMap { implicit orientation =>
          if (length == 1 && orientation == Col) {
            Seq()
          } else {
            concluded.lines.zipWithIndex
              .filter { case (_, lineIdx) => occInLine(lineIdx) >= length }
              .flatMap { case (lineMap, _) =>
              lineMap.toSeq.sliding(length)
                .filter(sl => sl.forall { case (_, c) => c != Water })
                .filterNot(sl => sl.forall { case (_, c) => c.isShip }) // dont set a ship again
                .filterNot(sl => sl.last._2.isShip && sl.last._2.asInstanceOf[Ship].isNextOpen.contains(true))
                .filterNot(sl => sl.head._2.isShip && sl.head._2.asInstanceOf[Ship].isPrevOpen.contains(true))
                .filterNot(_.tail.dropRight(1).exists{ case (_,c) => c.isShip && c.asInstanceOf[Ship].isStartEnd })
                .map { changeList =>
                  if (changeList.size == 1) {
                    Seq((changeList.head._1, Ship.SHIP_ONE))
                  } else {
                    Seq((changeList.head._1, Ship.start)) ++
                      changeList.tail.dropRight(1).map { case (p: Pos, _) => (p, Ship.knownMiddle) } ++
                      Seq((changeList.last._1, Ship.end))
                  }
                }
            }
          }
        }
      }

      shipSettingChanges
    }
  }

  lazy val rulesSatisfied: Boolean = {
    // Anzahl Schiffe in Zeilen und Spalten
    val rowsShipsOK = shipsInRows.zip(occInRows).forall( x => x._1 <= x._2 )
    lazy val colsShipsOK = shipsInCols.zip(occInCols).forall( x => x._1 <= x._2 )

    // nicht zu viel Wasser in Zeile/Spalte
    val rowsWaterOK = rowCells.map(_.count(_ == Water)).zip(occInRows).forall(x => x._1 <= size-x._2)
    lazy val colsWaterOK = colCells.map(_.count(_ == Water)).zip(occInCols).forall(x => x._1 <= size-x._2)

    val rowsOK = rowsShipsOK && rowsWaterOK
    lazy val colsOK = colsShipsOK && colsWaterOK

    // freie Felder um Schiffe
    lazy val diagonalSpaceOK = {
      state.filter(_._2.isShip)
           .forall { case (pos, _) => pos.diagonals.collect(state).forall(!_.isShip) }
    }

    lazy val endsOK = {
      state.collect{ case (p, c:Ship) if c.isPredefinedShip => (p,c) }.forall { case (pos, c) =>
        if (c.isKnownDirection) {
          val posAndOpen = (pos.leftAndRight ++ pos.upAndDown).zip(Seq(c.isLeftOpen,c.isRightOpen,c.isUpOpen,c.isDownOpen).map(_.get))
          val onlyKnown = posAndOpen.filter{ case (p, _) => state.get(p).exists(_.isKnown) }
          val ok = onlyKnown.forall{ case (p, isOpen) => state(p).isShip == isOpen }
          ok

        } else {
          val leftRight = pos.leftAndRight.collect(state).collect{ case c:Known => c.isShip } //.flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
          val leftRightEqualOrUnknown = leftRight.distinct.size <= 1
          val upDown = pos.upAndDown.collect(state).collect{ case c:Known => c.isShip } // .flatMap(state.get(_).map(_.isShip)).filter(_.isDefined).map(_.get)
          val upDownEqualOrUnknown = upDown.distinct.size <= 1

          val ok = leftRightEqualOrUnknown && upDownEqualOrUnknown && leftRight.intersect(upDown).isEmpty
          ok
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
    val allKnown = state.forall(_._2 != Unknown)

    // Anzahl Schiffe in Zeilen und Spalten
    lazy val rowsShipsOK = shipsInRows.zip(occInRows).forall( x => x._1 == x._2 )
    lazy val colsShipsOK = shipsInCols.zip(occInCols).forall( x => x._1 == x._2 )

    // Anzahl Schiffe in richtiger Grösse
    lazy val shipsOK = findShips._1 == ships

    allKnown && rowsShipsOK && colsShipsOK && shipsOK && rulesSatisfied
  }

  lazy val uniqueID: ByteBuffer = ByteBuffer.wrap(state.values.map {
    case Unknown => 0
    case Water => 1
    case _ => 2
  }.grouped(4).map(_.zipWithIndex).map(_.map(x => x._1 << x._2*2).sum.toByte).toArray)

  def solveWith(possibilitiesIndex: Int): BimaruBoard with SolverHelper = {
    updated(possibleSteps(possibilitiesIndex))
  }

  final def solveWith(possibilitiesIndices: Int*): BimaruBoard with SolverHelper = {
    if (possibilitiesIndices.isEmpty) this
    else solveWith(possibilitiesIndices.head).solveWith(possibilitiesIndices.tail: _*)
  }

  override def updated(changes: Seq[(Pos,Cell)]) = {
    val newState = super.updated(changes).state
    new BimaruBoard(ships, occInRows, occInCols, newState) with SolverHelper
  }

  lazy val findShips: (Map[Int,Int], Set[Pos]) = {
    var foundShips:Map[Int,Int] = ships.mapValues(_ => 0)
    var usedFields = Set[Pos]()

    Seq(Row, Col).foreach{ implicit orientation =>
      lines.foreach { lineMap =>
        var shipLength = 0
        lineMap.withFilter(posCell => !usedFields.contains(posCell._1)).foreach{ case (pos, cell) =>
          if (cell.isShip) {
            val notInLine = pos.notInLine.collect(state)
            if (notInLine.forall(_ == Water)) {
              shipLength += 1

              // if i'm last field in line, count ship!
              if (!state.contains(pos.next) && state.get(pos.prev(shipLength)).forall(_ == Water)) {
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
            if (shipLength > 0 && cell == Water && state.get(pos.prev(shipLength+1)).forall(_ == Water)) {
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

trait SolverHelperConcluded extends SolverHelper {
  override lazy val concluded: BimaruBoard with SolverHelper = this
}