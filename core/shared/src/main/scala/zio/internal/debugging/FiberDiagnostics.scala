package zio.internal.debugging

import zio.{ Fiber, IO }
import zio.internal.Stack
import zio.internal.stacktracer.ZTraceElement

case class FiberDiagnostics private[zio] (
  fiberId: Fiber.Id,
  value: Any,
  k: Any => IO[Any, Any],
  kTrace: ZTraceElement,
  stack: Stack[Any => IO[Any, Any]],
  stackTrace: List[ZTraceElement],
  execTrace: List[ZTraceElement],
  private[zio] val unfreeze: Runnable
)
