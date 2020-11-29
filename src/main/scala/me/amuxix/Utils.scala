package me.amuxix

object Utils {
  def isOrAre(t: Iterable[_]): String = if (t.size == 1) "is" else "are"
}
