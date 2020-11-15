package me.amuxix

package object syntax {
  object chaining extends ChainingSyntax
  object jda extends JDASyntax
  object action extends ActionSyntax

  object all extends ChainingSyntax with JDASyntax with ActionSyntax
}
