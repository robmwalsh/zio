package zio.internal.debugging

import java.lang
import java.util.concurrent.ConcurrentHashMap

import zio.Fiber
import zio.internal.stacktracer.ZTraceElement
import zio.internal.tracing.ZIOFn
//import zio.internal.tracing.ZIOFn.unwrap

import scala.collection.mutable.ListBuffer
import scala.io.StdIn
import scala.util.Try

object Debugger {

  import SourceHelper._
  import LayoutHelper._

  //todo check debugging is supported and is running (-agentlib:jdwp)
  @volatile
  private var debuggingEnabled: Boolean = true
  private[zio] def isEnabled: Boolean   = debuggingEnabled
  //flag if debugger is active (i.e. something is frozen)
  @volatile
  private var debuggerActive = false

  //flag if all fibers should freeze
  @volatile
  private[this] var frozen: Boolean  = true
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
      mode = Mode.Fiber(id)
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
                      s"${diagnostic.fiberId.startTimeMillis},${yellow(exactlyN(diagnostic.fiberId.seqNumber.toString, 7))}"
                    val v = red(exactlyN(diagnostic.value.toString, 10))
                    val source =
                      green(
                        exactlyN(
                          getTraceSourceHead(diagnostic.kTrace) match {
                            case Some(line) => line.line.trim
                            case None       => "source not found"
                          },
                          100
                        )
                      )
                    val location = green(diagnostic.kTrace match {
                      case ZTraceElement.NoLocation(error)                   => error
                      case ZTraceElement.SourceLocation(file, _, _, from, _) => s"($file:$from)"
                    })
                    val stackSize = cyan(exactlyN(diagnostic.stack.size.toString, 5))

                    println(s"Id:${id}s:${stackSize}v:${v}=>$source$location")
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
    val source = getTraceSource(diagnostics.kTrace, 0) match { //todo show context
      case Some(list) =>
        list.map(line => s"${line.lineNumber} ${line.line}").mkString("\n") //todo support windows
      case None => "source not found"
    }
    flush()
    println(s"fiberId         : ${diagnostics.fiberId}")
    println(s"value           : ${value}")

    val execTrace = diagnostics.execTrace.take(15).map { trace =>
      val source =
        exactlyN(
          getTraceSourceHead(trace) match {
            case Some(line) => line.line.trim
            case None       => "source not found"

          },
          100
        )

      val location = trace match {
        case ZTraceElement.NoLocation(error)                   => red(error)
        case ZTraceElement.SourceLocation(file, _, _, from, _) => green(s"($file:$from)")
      }
      s"$source$location"
    }
    println()
    println(green("execution trace:"))
    println(execTrace.mkString("\n")) //todo support windows

    println
    println(s"${red("next")} real: ${yellow(diagnostics.tracer.traceLocation(diagnostics.k).prettyPrint)} underlying ${green(diagnostics.kTrace.prettyPrint)}")
    println(source)

    println
    println(green("stack:"))

    val peekStack = diagnostics.stack.peekN(30).map { real =>
      val realLocation       = diagnostics.tracer.traceLocation(real)
      val underlyingLocation = diagnostics.tracer.traceLocation(ZIOFn.unwrap(real))

      s"${yellow(exactlyN(realLocation.prettyPrint, 70))} ${green(exactlyN(underlyingLocation.prettyPrint, 70))}"
    }

    println(peekStack.mkString("\n")) //todo support windows
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
