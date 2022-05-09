package me.amuxix.syntax

trait ChainingSyntax:
  @inline implicit final def chainingOps[A](a: A): ChainingOps[A] = new ChainingOps(a)

final class ChainingOps[A](private val self: A) extends AnyVal:
  def when(cond: Boolean)(f: A => A): A = if cond then f(self) else self
