# DottyLucre

[![Build Status](https://travis-ci.org/Sciss/DottyLucre.svg?branch=main)](https://travis-ci.org/Sciss/DottyLucre)

## statement

A sandbox project to experiment with ways of porting Lucre to Dotty (Scala 3). This is currently not possible without major rewriting,
because for academic reasons type projections were removed, which are a central element in Lucre.

This project is (C)opyright 2020 by Hanns Holger Rutz. All rights reserved. It is released under 
the [GNU Affero General Public License](https://raw.github.com/Sciss/DottyLucre/main/LICENSE) and comes with 
absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`

## requirements / installation

The project builds with sbt against Scala 2.13, Dotty 0.26.0-RC1.

## translation

We now come to settle on the following rewrites:

- `S <: Sys[S]` -> `T <: Txn[T]` and `[S]` -> `[T]`
- `S#Tx` -> `T`
- `read(in: DataInput, access: S#Acc)(implicit tx: S#Tx)` ->
  `read(in: DataInput, tx: T)(implicit acc: tx.Acc)`
- `Serializer[S#Tx, S#Acc, Form[S]]` -> `TSerializer[T, Form[T]]`
- `S#Id` -> `Ident[T]`
