import scala.collection.immutable.TreeMap

class BimaruBoard(val ships:Map[Int, Int], val occInRows:Seq[Int], val occInCols:Seq[Int], val state:TreeMap[Pos, Cell]) {

  // create board full of unknown fields
  def this(ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int]) = {
    this(ships, occInRows, occInCols,
      TreeMap((for (x<-1 to occInCols.length; y<-1 to occInRows.length) yield {
        Pos(x,y) -> Unknown
      }).toArray:_*)
    )
  }

  def size: Int = {
    if (occInRows.length != occInCols.length) throw new IllegalArgumentException("field has to be a square")
    occInRows.length
  }

  lazy val rows:Seq[TreeMap[Pos,Cell]] = BimaruBoard.rows(state)
  lazy val rowCells:Seq[Seq[Cell]] = rows.map(_.values.toSeq)
  lazy val cols:Seq[Map[Pos,Cell]] = BimaruBoard.cols(state)
  lazy val colCells:Seq[Seq[Cell]] = cols.map(_.values.toSeq)
  def lines(implicit l:LineOrientation): Seq[Map[Pos,Cell]] = l match {
    case Row => rows
    case Col => cols
  }
  def occInLine(idx: Int)(implicit orientation: LineOrientation): Int = orientation match {
    case Row => occInRows(idx)
    case Col => occInCols(idx)
  }

  lazy val shipsInRows: Seq[Int] = {
    rowCells.map(BimaruBoard.shipsIn)
  }
  lazy val shipsInCols: Seq[Int] = {
    colCells.map(BimaruBoard.shipsIn)
  }

  override lazy val toString: String = {
    rowCells.map(_.map( _.toString).mkString("|","|","|")).mkString("\n")
  }

  def updated(pos:Pos, cell:Cell): BimaruBoard = updated(List((pos,cell)))

  def updated(changes: Seq[(Pos,Cell)]): BimaruBoard = {
    var newState = state
    for { (pos,cell) <- changes } {
      val existing = newState(pos)
      assert(!existing.isKnown || existing.isShip == cell.isShip,
        s"tried to change water/ship-state of already known $pos, changeset: $changes")

      newState = newState.updated(pos,cell)
    }

    new BimaruBoard(ships, occInRows, occInCols, newState)
  }
}

object BimaruBoard {
  def apply(ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int], state:TreeMap[Pos, Cell]): BimaruBoard = {
    val board = new BimaruBoard(ships, occInRows, occInCols)
    var newState = board.state
    for ((p, c) <- state) {
      newState = newState.updated(p,c)
    }
    new BimaruBoard(ships, occInRows, occInCols, newState)
  }

  def rows(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.y).toSeq.sortBy(_._1).map(_._2)

  def cols(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.x).toSeq.sortBy(_._1).map(_._2)

  def shipsIn(row: Map[Pos,Cell]): Int = shipsIn(row.values)

  def shipsIn(row: Iterable[Cell]): Int = row.count(_.isShip)

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

  def printState(state: TreeMap[Pos,Cell]): String = {
    rows(state).map(_.values.toSeq).map(_.map( _.toString).mkString("|","|","|")).mkString("\n")
  }
}