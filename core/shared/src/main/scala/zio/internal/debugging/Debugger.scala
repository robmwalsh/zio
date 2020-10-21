package zio.internal.debugging

import java.lang
import java.util.concurrent.ConcurrentHashMap

import zio.Fiber
import zio.internal.stacktracer.ZTraceElement

import scala.io.{ Source, StdIn }

object Debugger {

  lazy val debuggingEnabled: Boolean = true

  //flag if all fibers should freeze
  @volatile
  private[this] var frozen: Boolean = false

  private[this] type FiberSet     = ConcurrentHashMap.KeySetView[Fiber.Id, lang.Boolean]
  private[this] type FrozenFibers = ConcurrentHashMap[Fiber.Id, FiberDiagnostics]
  private[this] type TraceSources = ConcurrentHashMap[ZTraceElement, String]

  //fibers that must freeze
  private[this] lazy val freezeFibers: FiberSet = ConcurrentHashMap.newKeySet[Fiber.Id](100)

  //trace sources
  private[this] lazy val traceSources: TraceSources = new TraceSources(10)

  val _ = traceSources
  //fibers that have been frozen
  private[this] lazy val frozenFibers: FrozenFibers = new FrozenFibers(10)

  //fibers that can run when debugger has frozen
  private[this] lazy val permittedFibers: FiberSet = ConcurrentHashMap.newKeySet[Fiber.Id](100)

  private[zio] def isFrozen: Boolean = frozen

  private[zio] def freezeAll(): Unit = {
    debugLoop()
    frozen = true
  }

  lazy val sources = s"${System.getProperty("user.home")}/IdeaProjects/zio/core/shared/src/main/scala/"

  def colored(code: String)(str: String): String = s"$code$str${Console.RESET}"
  lazy val red: String => String                 = colored(Console.RED)
  lazy val green: String => String               = colored(Console.GREEN)

  private def sourceSnippet(trace: ZTraceElement): String = {
    val res = traceSources.get(trace)
    if (res eq null) {
      val v = trace match {
        case ZTraceElement.NoLocation(_) => trace.prettyPrint
        case ZTraceElement.SourceLocation(sourceFile, clazz, _, from, to) =>
          val fileName  = sources + clazz.split('.').dropRight(1).mkString("/") + "/" + sourceFile
          val file      = Source.fromFile(fileName)
          val fileLines = file.getLines().toList
          file.close()
          val zipped    = fileLines.zipWithIndex
          val start     = from - 3
          val length    = to - from
          val lineCount = fileLines.length

          val drop = if (from > 3) start else 0
          val take =
            if (length > 10) 10
            else if (length + 3 > lineCount) lineCount - to
            else length + 6

          val limit = zipped.slice(drop, drop + take)
          val result = limit.map { case (line, lineNumber) =>
            s"${lineNumber + 1} ${if (lineNumber + 1 == from) "*" else " "} $line"
          }

          result.mkString("\n")
      }
      traceSources.put(trace, v)
      v
    } else {
      res
    }
  }

  private def debugLoop() = new Thread {
    override def run: Unit = {
      var done = false
      while (!done) {
        println(s"What do you want to do?")
        StdIn.readLine() match {
          case "wait" => Thread.sleep(1000)
          case "list" =>
            frozenFibers.forEach { (_, diagnostics) =>
              println(s"fiberId: ${diagnostics.fiberId} current value: ${diagnostics.value}")
            }
          case "inspect" =>
            frozenFibers.forEach { (_, diagnostics) =>
              println("-------------------------------------------")
              println(s"fiberId      : ${diagnostics.fiberId}")
              println(s"current value: ${red(diagnostics.value.toString)}")
              println("next instruction: ")
              println(sourceSnippet(diagnostics.kTrace))
              println("-------------------------------------------")
            }
          case "step" =>
            stepAll()
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
    stepAll()
  }

  private[zio] def stepAll(): Unit =
    frozenFibers.forEach { (_, diagnostics) =>
      diagnostics.unfreeze.run()
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
