/*
 *  GraphFormatMixin.scala
 *  (Lucre 4)
 *
 *  Copyright (c) 2009-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.impl

import de.sciss.lucre.expr.graph.Control
import de.sciss.serial.{DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

trait GraphFormatMixin {
  protected final def writeControls(controls: Vec[Control.Configured],
                                    out: DataOutput, ref0: ExElem.RefMapOut): ExElem.RefMapOut = {
    var ref = ref0
    out.writeInt(controls.size)
    controls.foreach { conf =>
      ref = ExElem.write(conf.control, out, ref)
      val m = conf.properties
      out.writeInt(m.size)
      m.foreach { case (key, v) =>
        out.writeUTF(key)
        ref = ExElem.write(v, out, ref)
      }
    }
    ref
  }

  protected final def readControls(in: DataInput, ref: ExElem.RefMapIn): Vec[Control.Configured] = {
    val szC = in.readInt()
    val cxb = Vec.newBuilder[Control.Configured]
    cxb.sizeHint(szC)
    var i = 0
    while (i < szC) {
      val ctl = ExElem.read(in, ref).asInstanceOf[Control]
      val mSz = in.readInt()
      val mb  = Map.newBuilder[String, Any]
      mb.sizeHint(mSz)
      var j = 0
      while (j < mSz) {
        val k = in.readUTF()
        val v = ExElem.read(in, ref)
        mb += k -> v
        j  += 1
      }
      val properties = mb.result()
      val configured = Control.Configured(ctl, properties)
      cxb += configured
      i   += 1
    }
    cxb.result()
  }
}

