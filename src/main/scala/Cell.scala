/**
 * models the state of a single cell on a bimaruboard
 * @param isWater none: the field is as of yet unknown
 * @param isLeftOpen none: no idea, false: left of this ship must be water, true: this field is not a '<' or 'o' (but could be '|')
 * @param isUpOpen none: no idea, false: above this ship must be water, true: this field is not a '^' or 'o' (but could be '=')
 * @param isRightOpen none: no idea, false: right of this ship must be water, true: this field is not a '>' or 'o' (but could be '|')
 * @param isDownOpen none: no idea, false: under this ship must be water, true: this field is not a 'v' or 'o' (but could be '=')
 */
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
  lazy val isStartEnd:Boolean = isPredefinedShip && List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).count(_.get) == 1

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
      case Cell.SHIP_VERT_START => "^"
      case Cell.SHIP_HORIZ_END => ">"
      case Cell.SHIP_HORIZ_START => "<"
      case Cell.SHIP_VERT_END => "v"
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
  lazy val SHIP = new Cell(false) // _could be_ a start or end

  lazy val SHIP_ONE = new Cell(Option(false), Option(false), Option(false), Option(false), Option(false))
  lazy val SHIP_MIDDLE = new Cell(Option(false), Option(true), Option(true), Option(true), Option(true)) // NOT start/end

  lazy val SHIP_HORIZ = new Cell(Option(false), None, Option(false), None, Option(false))
  lazy val SHIP_HORIZ_START = new Cell(Option(false), Option(false), Option(false), Option(true), Option(false))
  lazy val SHIP_HORIZ_END = new Cell(Option(false), Option(true), Option(false), Option(false), Option(false))

  lazy val SHIP_VERT = new Cell(Option(false), Option(false), None, Option(false), None)
  lazy val SHIP_VERT_START = new Cell(Option(false), Option(false), Option(false), Option(false), Option(true))
  lazy val SHIP_VERT_END = new Cell(Option(false), Option(false), Option(true), Option(false), Option(false))

  def start(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_HORIZ_START
    case Col => SHIP_VERT_START
  }

  def end(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_HORIZ_END
    case Col => SHIP_VERT_END
  }

  def knownMiddle(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => SHIP_HORIZ
    case Col => SHIP_VERT
  }
}
