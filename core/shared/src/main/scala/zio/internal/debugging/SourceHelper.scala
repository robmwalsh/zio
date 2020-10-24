package zio.internal.debugging

import java.util.concurrent.ConcurrentHashMap

import zio.internal.debugging.Debugger.sources
import zio.internal.debugging.SourceHelper.File
import zio.internal.stacktracer.ZTraceElement

import scala.io.Source
import scala.util.Try

case class Line(lineNumber: Int, line: String, fileName: File)
object Line {
  def apply(lineNumber: Int, line: String, fileName: File): Line =
    new Line(lineNumber, line.intern, fileName.intern)
}

object SourceHelper {
  type File = String

  private[zio] def getTraceSource(trace: ZTraceElement, contextOffset: Int): Option[List[Line]] =
    trace match {
      case ZTraceElement.NoLocation(_) => None
      case sourceLocation @ ZTraceElement.SourceLocation(sourceFile, _, _, from, to) =>
        val fileName: File = sources + sourceLocation.folder + sourceFile
        SourceFileCache.get(fileName) match {
          case Some(source) =>
            val res = for {
              lineNumber <- from - contextOffset until to + contextOffset + 1
              line       <- source.get(lineNumber)
            } yield line
            Some(res.toList)
          case None => None
        }
    }

  private[zio] def getTraceSource(trace: ZTraceElement): Option[List[Line]] = getTraceSource(trace, 0)

  private[zio] def getTraceSourceHead(trace: ZTraceElement): Option[Line] =
    trace match {
      case ZTraceElement.NoLocation(_) => None
      case sourceLocation @ ZTraceElement.SourceLocation(sourceFile, _, _, from, _) =>
        val fileName = sources + sourceLocation.folder + sourceFile
        for {
          source <- SourceFileCache.get(fileName)
          line   <- source.get(from)
        } yield line
    }

  object SourceFileCache {
    private[this] type SourceLinesCache = ConcurrentHashMap[File, Option[Map[Int, Line]]]
    private[this] lazy val sourceLinesCache: SourceLinesCache = new SourceLinesCache(10)

    private[zio] def get(fileName: File): Option[Map[Int, Line]] = {
      val sourceLines = sourceLinesCache.get(fileName)

      if (sourceLines eq null) {
        Try(Source.fromFile(fileName)).toOption match {

          case Some(file) =>
            Try("" :: file.getLines().toList).toOption match {

              case Some(fileLines) =>
                file.close()
                val tuples = fileLines.zipWithIndex.drop(1).map { case (rawLine, lineNumber) =>
                  lineNumber -> Line(lineNumber, rawLine, fileName)
                }
                val lineMap = Map(tuples: _*)
                sourceLinesCache.put(fileName, Some(lineMap))
                Some(lineMap)

              case None =>
                sourceLinesCache.put(fileName, None)
                None
            }

          case None =>
            sourceLinesCache.put(fileName, None)
            None
        }

      } else sourceLines
    }
  }
}
