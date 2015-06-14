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
  lazy val notDiagonals = leftAndRight ++ upAndDown

  def prev(implicit line: LineOrientation) = line match {
    case Row => left
    case Col => up
  }

  def prev(steps: Int)(implicit line: LineOrientation): Pos = {
    assert(steps >= 0)
    if (steps == 0) this
    else prev.prev(steps - 1)
  }

  def next(implicit line: LineOrientation): Pos = line match {
    case Row => right
    case Col => down
  }

  def inLine(implicit line: LineOrientation): Seq[Pos] = line match {
    case Row => leftAndRight
    case Col => upAndDown
  }

  def notInLine(implicit line: LineOrientation): Seq[Pos] = inLine(line.not)

  def pos(implicit orientation: LineOrientation): Int = orientation match {
    case Row => x
    case Col => y
  }

  def idx(implicit orientation: LineOrientation): Int = pos - 1
  def linePos(implicit orientation: LineOrientation): Int = pos(orientation.not)
  def lineIdx(implicit orientation: LineOrientation): Int = linePos - 1

  def orientationTo(otherPos: Pos): Option[LineOrientation] = {
    if (otherPos == this) None
    else if (otherPos.x == this.x) Some(Col)
    else if (otherPos.y == this.y) Some(Row)
    else None
  }
}
