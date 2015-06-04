sealed trait LineOrientation {
  def not: LineOrientation
}

object Row extends LineOrientation {
  override def not = Col
}
object Col extends LineOrientation {
  override def not = Row
}