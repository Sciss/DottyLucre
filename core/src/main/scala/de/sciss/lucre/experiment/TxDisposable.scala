package de.sciss.lucre.experiment

trait TxDisposable[-Tx] {
  def dispose()(implicit tx: Tx): Unit
}
