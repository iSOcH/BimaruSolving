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

  def isPrevOpen(implicit l: LineOrientation): Option[Boolean] = l match {
    case Row => isLeftOpen
    case Col => isUpOpen
  }

  def isNextOpen(implicit l: LineOrientation): Option[Boolean] = l match {
    case Row => isRightOpen
    case Col => isDownOpen
  }

  override lazy val toString: String = isShip match {
    case Some(false) => "~"
    case None => " "
    case _ => this match {
      case Cell.SHIP_ONE => "o"
      case Cell.SHIP_START_DOWN => "^"
      case Cell.SHIP_START_LEFT => ">"
      case Cell.SHIP_START_RIGHT => "<"
      case Cell.SHIP_START_UP => "v"
      case Cell.SHIP_MIDDLE => "+"
      case Cell.SHIP_HORIZ => "="
      case Cell.SHIP_VERT => "|"
      case Cell.SHIP => "+"
    }
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

  def start(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_START_RIGHT
    case Col => SHIP_START_DOWN
  }

  def end(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_START_LEFT
    case Col => SHIP_START_UP
  }

  def knownMiddle(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_HORIZ
    case Col => SHIP_VERT
  }
}
