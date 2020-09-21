package de.sciss.lucre

trait ProductWithAdjuncts extends Product {
  def adjuncts: List[Adjunct]
}
