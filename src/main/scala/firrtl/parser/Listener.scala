// SPDX-License-Identifier: Apache-2.0

package firrtl.parser

import firrtl.antlr.{FIRRTLParser, _}
import firrtl.Visitor
import firrtl.Parser.InfoMode
import firrtl.ir._
import firrtl.UnsupportedVersionException

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

private[firrtl] class Listener(infoMode: InfoMode) extends FIRRTLBaseListener {
  private var main: Option[String] = None
  private var info: Option[Info] = None
  private val modules = mutable.ArrayBuffer.empty[DefModule]

  private val visitor = new Visitor(infoMode)

  override def exitModule(ctx: FIRRTLParser.ModuleContext): Unit = {
    val m = visitor.visitModule(ctx)
    ctx.children = null // Null out to save memory
    modules += m
  }

  override def exitCircuit(ctx: FIRRTLParser.CircuitContext): Unit = {
    info = Some(visitor.visitInfo(Option(ctx.info), ctx))
    main = Some(ctx.id.getText)
    visitor.visitVersion(Option(ctx.version), ctx) match {
      case Some(version) => {
        val parts = version.split("\\.")
        val (major, minor) = (parts(0).toInt, parts(1).toInt)
        if (major > 1 || (major == 1 && minor > 1)) {
          throw new UnsupportedVersionException(s"FIRRTL version ${version} is not supported (greater than 1.1.x)")
        }
      }
      case _ =>
    }
    ctx.children = null // Null out to save memory
  }

  def getCircuit: Circuit = {
    require(main.nonEmpty)
    val mods = modules.toSeq
    Circuit(info.get, mods, main.get)
  }
}
