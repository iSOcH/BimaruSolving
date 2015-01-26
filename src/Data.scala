case class Pos(x:Int, y:Int) extends Comparable[Pos] {
  override def compareTo(other:Pos): Int = compare(other)

  def compare(other:Pos): Int = {
    if (y != other.y) {
      y - other.y
    } else {
      x - other.x
    }
  }

  lazy val up = Pos(x, y-1)
  lazy val down = Pos(x, y+1)
  lazy val left = Pos(x-1, y)
  lazy val right = Pos(x+1, y)
  lazy val diagonals = Seq(up.left, up.right, down.left, down.right)
  lazy val upAndDown = Seq(up, down)
  lazy val leftAndRight = Seq(left, right)
}

case class Cell private (isWater:Option[Boolean], isLeftOpen:Option[Boolean],
                         isUpOpen:Option[Boolean], isRightOpen:Option[Boolean],
                         isDownOpen:Option[Boolean]){

  // empty field
  private def this() = this(None, None, None, None, None)
  private def this(isWater:Boolean) = this(Some(isWater), None, None, None, None)

  lazy val isKnown:Boolean = isWater.isDefined
  lazy val isShip:Option[Boolean] = isWater.map(! _)
  lazy val isPredefinedShip:Boolean = isKnown && List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).forall(_.isDefined)
  lazy val isKnownDirection:Boolean = isPredefinedShip && List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).count(_.get) <= 2

  override lazy val toString: String = isShip match {
    case Some(true) => "X"
    case Some(false) => "~"
    case None => " "
  }
}

object Cell {
  lazy val UNKNOWN = new Cell()
  lazy val WATER = new Cell(true)
  lazy val SHIP = new Cell(false)
  lazy val SHIP_ONE = new Cell(Option(false), Option(false), Option(false), Option(false), Option(false))
  lazy val SHIP_MIDDLE = new Cell(Option(false), Option(true), Option(true), Option(true), Option(true))
  lazy val SHIP_START_LEFT = new Cell(Option(false), Option(true), Option(false), Option(false), Option(false))
  lazy val SHIP_START_RIGHT = new Cell(Option(false), Option(false), Option(false), Option(true), Option(false))
  lazy val SHIP_START_UP = new Cell(Option(false), Option(false), Option(true), Option(false), Option(false))
  lazy val SHIP_START_DOWN = new Cell(Option(false), Option(false), Option(false), Option(false), Option(true))
  lazy val SHIP_HORIZ = new Cell(Option(false), Option(true), Option(false), Option(true), Option(false))
  lazy val SHIP_VERT = new Cell(Option(false), Option(false), Option(true), Option(false), Option(true))
}