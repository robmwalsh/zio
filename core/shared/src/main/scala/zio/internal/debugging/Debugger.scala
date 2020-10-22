package zio.internal.debugging

import java.lang
import java.util.concurrent.ConcurrentHashMap

import zio.Fiber
import zio.internal.stacktracer.ZTraceElement

import scala.collection.mutable.ListBuffer
import scala.io.{ Source, StdIn }
import scala.util.Try

object Debugger {

  lazy val debuggingEnabled: Boolean = true

  @volatile
  private var debuggerRunning = false

  //flag if all fibers should freeze
  @volatile
  private[this] var frozen: Boolean = false

  private[this] type FiberSet     = ConcurrentHashMap.KeySetView[Long, lang.Boolean]
  private[this] type FrozenFibers = ConcurrentHashMap[Long, FiberDiagnostics]
  private[this] type TraceSources = ConcurrentHashMap[ZTraceElement, String]

  //fibers that must freeze
  private[this] lazy val freezeFibers: FiberSet = ConcurrentHashMap.newKeySet[Long](100)

  //trace sources
  private[this] lazy val traceSourceSnippets: TraceSources = new TraceSources(10)
  private[this] lazy val traceSourceLines: TraceSources    = new TraceSources(10)

  //fibers that have been frozen
  private[this] lazy val frozenFibers: FrozenFibers = new FrozenFibers(10)

  //fibers that can run when debugger has frozen
  private[this] lazy val permittedFibers: FiberSet = ConcurrentHashMap.newKeySet[Long](100)

  private[zio] def isFrozen: Boolean = frozen

  private[zio] def freezeAll(): Unit = {
    debugLoop()
    frozen = true
  }

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

  private def sourceLine(trace: ZTraceElement): String = {
    val res = traceSourceLines.get(trace)
    if (res eq null) {
      val _    = sourceSnippet(trace)
      val res2 = traceSourceLines.get(trace)
      if (res2 eq null) "something broke" else res2
    } else
      res

  }
  private def sourceSnippet(trace: ZTraceElement): String = {
    val res = traceSourceSnippets.get(trace)
    if (res eq null) {
      val v = trace match {
        case ZTraceElement.NoLocation(_) => trace.prettyPrint
        case ZTraceElement.SourceLocation(sourceFile, clazz, _, from, to) =>
          val fileName = sources + clazz.split('.').dropRight(1).mkString("/") + "/" + sourceFile
          Try(Source.fromFile(fileName)).fold(
            _ => s"$sourceFile not found",
            file =>
              Try("" :: file.getLines().toList).fold(
                _ => "something broke",
                fileLines => {
                  file.close()
                  val zipped    = fileLines.zipWithIndex.drop(1)
                  val start     = from - 3
                  val length    = to - from
                  val lineCount = fileLines.length

                  val drop = if (from > 3) start else 0
                  val take =
                    if (length > 10) 10
                    else if (length + 3 > lineCount) lineCount - to
                    else length + 6

                  val limit = zipped.slice(drop, drop + take)
                  val result = limit.map { case (rawLine, lineNumber) =>
                    val line = s"$lineNumber ${if (lineNumber == from) "-> " else "   "} $rawLine"
                    if (lineNumber == from) traceSourceLines.put(trace, rawLine.trim)
                    if (from <= lineNumber && lineNumber <= to)
                      green(line)
                    else line
                  }
                  result.mkString("\n")
                }
              )
          )
      }
      traceSourceSnippets.put(trace, v)
      v
    } else {
      res
    }
  }

  sealed trait Mode
  object Mode {
    final case object Overview        extends Mode
    sealed case class Fiber(id: Long) extends Mode
  }
  private var mode: Mode = Mode.Overview

  private val nls = "\n" * 50
  private def flush(): Unit =
    println(nls)

  private def debugLoop() =
    if (!debuggerRunning) {
      new Thread {
        override def run: Unit = {
          var done = false
          while (!done) {
            mode match {
              case Mode.Overview =>
                val list = new ListBuffer[FiberDiagnostics]()
                frozenFibers.forEach { (_, diagnostics) =>
                  val _ = list += diagnostics
                }
                flush()
                list
                  .sortBy(_.fiberId.seqNumber)
                  .foreach { diagnostic =>
                    val id =
                      s"Id:${diagnostic.fiberId.startTimeMillis},${yellow(exactlyN(diagnostic.fiberId.seqNumber.toString, 7))}"
                    val v = s" v: ${red(
                      exactlyN(diagnostic.value.toString, 10)
                    )}"
                    val source = s"${exactlyN(sourceLine(diagnostic.kTrace), 100)}"
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
              case maybeId =>
                Try(maybeId.toLong).map { id =>
                  mode = Mode.Fiber(id)
                  ()
                }
            }
          }
          debuggerRunning = false
        }
      }.start()
    }

  private def printDetail(diagnostics: FiberDiagnostics) = {
    val value = red(exactlyN(diagnostics.value.toString, 100))
    flush()
    println(s"fiberId         : ${diagnostics.fiberId}")
    println(s"value           : ${value}")
    println(s"location        : ${green(diagnostics.kTrace.prettyPrint)}")
    println(sourceSnippet(diagnostics.kTrace))
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
    debugLoop()
    permittedFibers.remove(fiberId)
    val _ = freezeFibers.add(fiberId.seqNumber)
  }

  private[zio] def freezeEvaluation(diagnostics: FiberDiagnostics): Unit = {
    frozenFibers.put(diagnostics.fiberId.seqNumber, diagnostics)
    val _ = freezeFibers.remove(diagnostics.fiberId.seqNumber)
  }

}
