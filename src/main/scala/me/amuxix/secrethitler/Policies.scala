package me.amuxix.secrethitler

import me.amuxix.secrethitler.{FascistPolicy => FascistPolicy, LiberalPolicy => LiberalPolicy, Policy}

import scala.util.Random

case class EnactedPolicies(liberal: Int, fascist: Int) {

  def enact(policy: Policy): EnactedPolicies = policy match {
    case LiberalPolicy => copy(liberal = liberal + 1)
    case FascistPolicy => copy(fascist = fascist + 1)
  }
}

case class Policies(
  private val drawPile: List[Policy] = Random.shuffle(Game.policyDeck),
  private val discardPile: List[Policy] = List.empty,
  enacted: EnactedPolicies = EnactedPolicies(0, 0),
  floating: List[Policy] = List.empty,
) {

  def drawPolicies(amount: Int = 3): Policies =
    if (floating.nonEmpty) {
      if (amount == floating.size) {
        this
      } else if (amount < floating.size) {
        val (newFloating, backToDeck) = floating.splitAt(amount)
        copy(floating = newFloating, drawPile = backToDeck ++ drawPile)
      } else { //amount > floating.size
        val (extraFloating, remainingDrawPile) = drawPile.splitAt(amount - floating.size)
        copy(floating = floating ++ extraFloating, drawPile = remainingDrawPile)
      }
    } else if (drawPile.size >= amount) {
      val (drawn, remainingDrawPile) = drawPile.splitAt(amount)
      copy(drawPile = remainingDrawPile, floating = drawn)
    } else {
      val (drawn, remainingDrawPile) = Random.shuffle(drawPile ++ discardPile).splitAt(amount)
      copy(drawPile = remainingDrawPile, discardPile = List.empty, floating = drawn)
    }

  private def discardPolicies(policies: Policy*): Policies =
    copy(discardPile = discardPile ++ policies, floating = floating diff policies)

  def discardPolicy(policy: Policy): Policies = discardPolicies(policy)

  def discardAll: Policies = discardPolicies(floating: _*)

  def enact(policy: Policy): Policies = copy(enacted = enacted.enact(policy))
}
