package zio.internal.debugging

import java.lang
import java.util.concurrent.ConcurrentHashMap

import zio.Fiber

import scala.io.StdIn

object Debugger {

  lazy val debuggingEnabled: Boolean = true

  //flag if all fibers should freeze
  @volatile
  private[this] var frozen: Boolean = false

  private[this] type FiberSet     = ConcurrentHashMap.KeySetView[Fiber.Id, lang.Boolean]
  private[this] type FrozenFibers = ConcurrentHashMap[Fiber.Id, FiberDiagnostics]

  //fibers that must freeze
  private[this] lazy val freezeFibers: FiberSet = ConcurrentHashMap.newKeySet[Fiber.Id](100)

  //fibers that have been frozen
  private[this] lazy val frozenFibers: FrozenFibers = new FrozenFibers(10000)

  //fibers that can run when debugger has frozen
  private[this] lazy val permittedFibers: FiberSet = ConcurrentHashMap.newKeySet[Fiber.Id](100)

  private[zio] def isFrozen: Boolean = frozen

  private[zio] def freezeAll(): Unit = {
    debugLoop()
    frozen = true
  }

  private def debugLoop() = new Thread {
    override def run: Unit = {
      var done = false
      while (!done) {
        println(s"What do you want to do?")
        StdIn.readLine() match {
          case "wait" => Thread.sleep(1000)
          case "list" => frozenFibers.forEach((_, diagnostics) => println(diagnostics.toString))
          case "inspect" =>
            val x = 42
            val _ = x //insert magical breakpoint here
          case "exit" =>
            done = true
            unfreezeAll()
          case _ =>
        }
      }
    }
  }.start()

  private[zio] def unfreezeAll(): Unit = {
    frozen = false
    frozenFibers.forEach { (_, diagnostics) =>
      diagnostics.continuation.run()
    }
  }

  private[zio] def executionPermitted(fiberId: Fiber.Id): Boolean = permittedFibers.contains(fiberId)

  private[zio] def freezeFiber(fiberId: Fiber.Id): Unit = {
    debugLoop()
    permittedFibers.remove(fiberId)
    val _ = freezeFibers.add(fiberId)
  }

  private[zio] def freezeEvaluation(diagnostics: FiberDiagnostics): Unit = {
    frozenFibers.put(diagnostics.fiberId, diagnostics)
    val _ = freezeFibers.remove(diagnostics.fiberId)
  }

}
