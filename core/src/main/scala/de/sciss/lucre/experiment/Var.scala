package de.sciss.lucre.experiment

import de.sciss.serial.Writable

trait Sink[-A] {
  def update(value: A): Unit
}

trait Var[A] extends Sink[A] with Writable with Disposable {
  def apply(): A
}
