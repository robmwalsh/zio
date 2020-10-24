package zio.internal.debugging

import java.lang
import java.util.concurrent.ConcurrentHashMap

import zio.Fiber
import zio.internal.stacktracer.ZTraceElement

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Try

object Debugger {

  //todo check debugging is supported and is running (-agentlib:jdwp)
  @volatile
  private var debuggingEnabled: Boolean = true
  private[zio] def isEnabled: Boolean   = debuggingEnabled
  //flag if debugger is active (i.e. something is frozen)
  @volatile
  private var debuggerActive = false

  //flag if all fibers should freeze
  @volatile
  private[this] var frozen: Boolean  = false
  private[zio] def isFrozen: Boolean = frozen //todo do I need to copy this?

  private[this] type FiberSet     = ConcurrentHashMap.KeySetView[Long, lang.Boolean]
  private[this] type FrozenFibers = ConcurrentHashMap[Long, FiberDiagnostics]

  //fibers that must freeze
  private[this] lazy val freezeFibers: FiberSet = ConcurrentHashMap.newKeySet[Long](100)
  //fibers that have been frozen
  private[this] lazy val frozenFibers: FrozenFibers = new FrozenFibers(10)
  //fibers that can run when debugger has frozen
  private[this] lazy val permittedFibers: FiberSet = ConcurrentHashMap.newKeySet[Long](100)

  private[zio] def freezeAll(id: Fiber.Id): Unit = {
    frozen = true
    Thread.sleep(100)
    debugLoop(id.seqNumber)
  }

  //todo fix
  lazy val sources = s"${System.getProperty("user.home")}/IdeaProjects/zio/core/shared/src/main/scala/"

  def colored(code: String)(str: String): String = s"$code$str${Console.RESET}"
  lazy val red: String => String                 = colored(Console.RED)
  lazy val green: String => String               = colored(Console.GREEN)
  lazy val yellow: String => String              = colored(Console.YELLOW)

  def exactlyN(string: String, n: Int): String =
    if (!string.isEmpty) {
      val first   = string.linesIterator.next()
      val trimmed = first.substring(0, Math.min(first.length(), n))
      val delta   = n - trimmed.length
      val padding = " " * delta
      trimmed + padding
    } else " " * n

  sealed trait Mode
  object Mode {
    final case object Overview        extends Mode
    sealed case class Fiber(id: Long) extends Mode
  }
  private var mode: Mode = Mode.Overview

  private val nls = "\n" * 50
  private def flush(): Unit =
    println(nls)

  private def debugLoop(id: Long) =
    if (!debuggerActive) {
      Mode.Fiber(id)
      new Thread {
        override def run: Unit = {
          var done = false
          while (!done && !Thread.currentThread().isInterrupted) {
            mode match {
              case Mode.Overview =>
                val list = new ListBuffer[FiberDiagnostics]()
                frozenFibers.forEach { (_, diagnostics) =>
                  val _ = list += diagnostics
                }
                flush()
                println("FrozenFibers:")
                list
                  .sortBy(_.fiberId.seqNumber)
                  .foreach { diagnostic =>
                    val id =
                      s"Id:${diagnostic.fiberId.startTimeMillis},${yellow(exactlyN(diagnostic.fiberId.seqNumber.toString, 7))}"
                    val v = s" v: ${red(
                      exactlyN(diagnostic.value.toString, 10)
                    )}"
                    val source =
                      s"${exactlyN(
                        SourceHelper.getTraceSourceHead(diagnostic.kTrace) match {
                          case Some(line) => line.line.trim
                          case None       => "source not found"
                        },
                        100
                      )}"
                    val location = s" ${green(diagnostic.kTrace match {
                      case ZTraceElement.NoLocation(error)                   => error
                      case ZTraceElement.SourceLocation(file, _, _, from, _) => s"($file:$from)"
                    })}"
                    println(s"$id$v => $source$location")
                  }
              case Mode.Fiber(id) =>
                val diagnostics: FiberDiagnostics = frozenFibers.get(id)
                if (diagnostics != null) {
                  printDetail(diagnostics)
                } else {
                  flush()
                  println("lost fiber, reverting to overview")
                  Thread.sleep(500)
                  mode = Mode.Overview
                }
            }
            StdIn.readLine() match {
              case "o" =>
                mode = Mode.Overview
              case "" =>
                mode match {
                  case Mode.Overview  => stepAll()
                  case Mode.Fiber(id) => stepFiber(id)
                }
                Thread.sleep(100)
              case "exit" =>
                done = true
                unfreezeAll()
              case "disable" =>
                debuggingEnabled = false
                unfreezeAll()
                Thread.sleep(1000)
              case maybeId =>
                Try(maybeId.toLong).map { id =>
                  mode = Mode.Fiber(id)
                  ()
                }
            }
          }
          debuggerActive = false
          unfreezeAll()
        }
      }.start()
    }

  private def printDetail(diagnostics: FiberDiagnostics) = {
    val value = red(exactlyN(diagnostics.value.toString, 100))
    val source = SourceHelper.getTraceSource(diagnostics.kTrace, 0) match { //todo show context
      case Some(list) =>
        list.map(line => s"${line.lineNumber} ${line.line}").mkString("\n") //todo support windows
      case None => "source not found"
    }
    flush()
    println(s"fiberId         : ${diagnostics.fiberId}")
    println(s"value           : ${value}")
    println(s"location        : ${green(diagnostics.kTrace.prettyPrint)}")
    println(source)
  }

  private[zio] def unfreezeAll(): Unit = {
    frozen = false
    stepAll()
  }

  private[zio] def stepFiber(id: Long): Unit = {
    val diagnostics = frozenFibers.get(id)
    if (diagnostics != null) {
      frozenFibers.remove(id)
      diagnostics.unfreeze.run()
    }
  }

  private[zio] def stepAll(): Unit = {
    frozen = true
    frozenFibers.forEach { (fiberId, diagnostics) =>
      frozenFibers.remove(fiberId)
      diagnostics.unfreeze.run()
    }
  }

  private[zio] def executionPermitted(fiberId: Fiber.Id): Boolean = permittedFibers.contains(fiberId)

  private[zio] def freezeFiber(fiberId: Fiber.Id): Unit = {
    permittedFibers.remove(fiberId)
    val _ = freezeFibers.add(fiberId.seqNumber)
    debugLoop(fiberId.seqNumber)
  }

  private[zio] def freezeEvaluation(diagnostics: FiberDiagnostics): Unit = {
    frozenFibers.put(diagnostics.fiberId.seqNumber, diagnostics)
    val _ = freezeFibers.remove(diagnostics.fiberId.seqNumber)
  }

  sealed trait BreakType
  object BreakType {
    final case object All   extends BreakType
    final case object Fiber extends BreakType
  }
}
