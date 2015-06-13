import scala.collection.immutable.TreeMap

class BimaruBoard(val size:Int, val ships:Map[Int, Int], val occInRows:Seq[Int], val occInCols:Seq[Int], val state:TreeMap[Pos, Cell]) {
  if (size < 2 || size % 2 != 0) throw new IllegalArgumentException("size must greater than 1 and dividable by 2")

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

  lazy val shipsInRows: Seq[Int] = {
    rowCells.map(BimaruBoard.shipsIn)
  }
  lazy val shipsInCols: Seq[Int] = {
    colCells.map(BimaruBoard.shipsIn)
  }
  
  override lazy val toString: String = {
    rowCells.map(_.map( _.toString).mkString("|","|","|")).mkString("\n")
  }

  // create board full of unknown fields
  def this(size:Int, ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int]) = {
    this(size, ships, occInRows, occInCols,
      TreeMap((for (x<-1 to size; y<-1 to size) yield {
          Pos(x,y) -> Cell.UNKNOWN
      }).toArray:_*)
    )
  }

  def updated(pos:Pos, cell:Cell): BimaruBoard = updated(List((pos,cell)))

  def updated(changes: Seq[(Pos,Cell)]): BimaruBoard = {
    var newState = state
    for ((p, c) <- changes) {newState = newState.updated(p,c)}
    new BimaruBoard(size, ships, occInRows, occInCols, newState)
  }

  def rows(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.y).toSeq.sortBy(_._1).map(_._2)

  def cols(state:TreeMap[Pos,Cell]): Seq[TreeMap[Pos,Cell]] = state.groupBy(_._1.x).toSeq.sortBy(_._1).map(_._2)

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
}

object BimaruBoard {
  def apply(size:Int, ships:Map[Int, Int], occInRows:Seq[Int], occInCols:Seq[Int], state:TreeMap[Pos, Cell]): BimaruBoard = {
    val board = new BimaruBoard(size, ships, occInRows, occInCols)
    board.updated(state.toSeq)
  }

  def shipsIn(row: Map[Pos,Cell]): Int = shipsIn(row.values)
  def shipsIn(row: Iterable[Cell]): Int = row.count(_.isShip.getOrElse(false))
}