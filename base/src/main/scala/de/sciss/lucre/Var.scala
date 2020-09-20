package de.sciss.lucre

import de.sciss.serial.Writable

trait Source[+A] {
  def apply(): A
}

trait Sink[-A] {
  def update(value: A): Unit
}

trait Var[A] extends Source[A] with Sink[A] with Writable with Disposable
