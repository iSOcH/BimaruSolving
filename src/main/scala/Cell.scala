sealed trait Cell {
  /**
   * @return true if ship, false if unknown or water
   */
  def isShip: Boolean

  def isKnown: Boolean
}

case object Unknown extends Cell {
  override val isKnown = false
  override val isShip = false
  override val toString = " "
}
sealed trait Known extends Cell {
  override val isKnown = true
}

case object Water extends Known {
  override val isShip = false
  override val toString = "~"
}

/**
 * models the state of a single cell on a bimaruboard
 * @param isLeftOpen none: no idea, false: left of this ship must be water, true: this field is not a '<' or 'o' (but could be '|')
 * @param isUpOpen none: no idea, false: above this ship must be water, true: this field is not a '&#94;' or 'o' (but could be '=')
 * @param isRightOpen none: no idea, false: right of this ship must be water, true: this field is not a '>' or 'o' (but could be '|')
 * @param isDownOpen none: no idea, false: under this ship must be water, true: this field is not a 'v' or 'o' (but could be '=')
 */
case class Ship private (isLeftOpen:Option[Boolean],
                         isUpOpen:Option[Boolean],
                         isRightOpen:Option[Boolean],
                         isDownOpen:Option[Boolean]) extends Known {

  override val isShip = true

  // ship without information
  private def this() = this(None, None, None, None)

  lazy val isPredefinedShip:Boolean = List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).forall(_.isDefined)
  lazy val isKnownDirection:Boolean = isPredefinedShip && List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).count(_.get) <= 2
  lazy val isStartEnd:Boolean = isPredefinedShip && List(isLeftOpen, isUpOpen, isDownOpen, isRightOpen).count(_.get) == 1

  def updatedPrevOpen(isPrevOpen: Boolean)(implicit o: LineOrientation): Ship = o match {
    case Row => copy(isLeftOpen = Some(isPrevOpen))
    case Col => copy(isUpOpen = Some(isPrevOpen))
  }

  def updatedPrevOpen(isPrevOpen: Option[Boolean])(implicit o: LineOrientation): Ship = isPrevOpen match {
    case None => this
    case Some(b) => updatedPrevOpen(b)
  }

  def updatedNextOpen(isNextOpen: Boolean)(implicit o: LineOrientation): Ship = o match {
    case Row => copy(isRightOpen = Some(isNextOpen))
    case Col => copy(isDownOpen = Some(isNextOpen))
  }

  def updatedNextOpen(isNextOpen: Option[Boolean])(implicit o: LineOrientation): Ship = isNextOpen match {
    case None => this
    case Some(b) => updatedNextOpen(b)
  }

  def isPrevOpen(implicit l: LineOrientation): Option[Boolean] = l match {
    case Row => isLeftOpen
    case Col => isUpOpen
  }

  def isNextOpen(implicit l: LineOrientation): Option[Boolean] = l match {
    case Row => isRightOpen
    case Col => isDownOpen
  }

  override lazy val toString: String = this match {
    case Ship.DEFAULT => "\u25A1"

    case Ship.ONE => "\u25CF"
    case Ship.MIDDLE => "\u25A0"

    case Ship.HORIZ => "\u25AD"
    case Ship.HORIZ_MIDDLE => "\u25AC"
    case Ship.HORIZ_START => "\u25C0"
    case Ship.HORIZ_END => "\u25B6"

    case Ship.VERT => "\u25AF"
    case Ship.VERT_MIDDLE => "\u25AE"
    case Ship.VERT_START => "\u25B2"
    case Ship.VERT_END => "\u25BC"

    case _ => "?"
  }
}

object Ship {
  lazy val DEFAULT = new Ship() // _could be_ a start or end

  lazy val ONE = new Ship(Option(false), Option(false), Option(false), Option(false))
  lazy val MIDDLE = new Ship(Option(true), Option(true), Option(true), Option(true)) // NOT start/end

  lazy val HORIZ = new Ship(None, Option(false), None, Option(false))
  lazy val HORIZ_MIDDLE = HORIZ.copy(isLeftOpen = Option(true), isRightOpen = Option(true))
  lazy val HORIZ_START = new Ship(Option(false), Option(false), Option(true), Option(false))
  lazy val HORIZ_END = new Ship(Option(true), Option(false), Option(false), Option(false))

  lazy val VERT = new Ship(Option(false), None, Option(false), None)
  lazy val VERT_MIDDLE = VERT.copy(isUpOpen = Option(true), isDownOpen = Option(true))
  lazy val VERT_START = new Ship(Option(false), Option(false), Option(false), Option(true))
  lazy val VERT_END = new Ship(Option(false), Option(true), Option(false), Option(false))

  def start(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => HORIZ_START
    case Col => VERT_START
  }

  def end(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => HORIZ_END
    case Col => VERT_END
  }

  def knownMiddle(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => HORIZ_MIDDLE
    case Col => VERT_MIDDLE
  }

  def inDirection(implicit orientation: LineOrientation): Cell = orientation match {
    case Row => HORIZ
    case Col => VERT
  }
}
