package me.amuxix

object Utils:
  def isOrAre(t: Iterable[?]): String = if t.size == 1 then "is" else "are"
