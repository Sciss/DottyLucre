# DottyLucre

[![Build Status](https://travis-ci.org/Sciss/DottyLucre.svg?branch=main)](https://travis-ci.org/Sciss/DottyLucre)

## statement

A sandbox project to experiment with ways of porting Lucre to Dotty (Scala 3). This required some major rewriting,
because type projections have been removed, which were a central element in Lucre.

This project is (C)opyright 2020 by Hanns Holger Rutz. All rights reserved. It is released under 
the [GNU Affero General Public License](https://raw.github.com/Sciss/DottyLucre/main/LICENSE) and comes with 
absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

This will be merged into a dedicated branch of the [upstream project](https://github.com/Sciss/Lucre) soon,
eventually becoming new major __version 4__ of the framework.

## requirements / installation

The project is intended to build with sbt against Scala 2.13, Dotty 0.27.0-RC1. The Dotty compiler currently crashes,
especially because of https://github.com/lampepfl/dotty/issues/9841. 
Hopefully cross-compilation is fixed with the next Dotty release.

## translation

We now come to settle on the following rewrites:

- `S <: Sys[S]` -> `T <: Txn[T]` and `[S]` -> `[T]`
- `S#Tx` -> `T`
- `read(in: DataInput, access: S#Acc)(implicit tx: S#Tx)` ->
  `readT(in: DataInput)(implicit tx: T)`
- `Serializer[S#Tx, S#Acc, Form[S]]` -> `TSerializer[T, Form[T]]`
- `S#Id` -> `Ident[T]`
- `S#Var` -> `Var[T]`
- `tx.newVar` and `tx.readVar` -> `id.newVar` and `id.readVar`

Further notes:

- the access parameter `S#Acc` has basically been removed from most API, including `Exec` which replaces
`Executor`, and also `Txn`. It is now only found in `confluent.Txn`.
- Where `Ident[T]` must be more specific as `tx.Id`, the `!` method can be used
- Likewise, where `Access[T]` must be more specific as `tx.Acc`, the `!` method can be used
- Transactionally _identified_ objects should no longer use a transaction method parameter `(implicit t: T)`,
  since by their nature they are only valid within the transaction that created or deserialised them. Thus internally,
  those objects typically carry a `tx: T` parameter.
- Transactionally _operating_ objects which can reside in memory, must still use a transaction parameter on their
  methods, of course. Often the (new) distinction is now reflected in the API, for example `id.newVar` creates a
  `Var[A]`, but `system.root` creates a `TVar[T, A]`, and `tx.newHandle` creates a `TSource[T, A]`. A transactionally
  identified object will use `Disposable`, whereas a transactionally operating object residing in memory will use
  `TDisposable[T]`. 
- common API is now "flat" in the `de.sciss.lucre` package, while some specialised API is still
  inside sub-packages such as `data` and `geom`.
