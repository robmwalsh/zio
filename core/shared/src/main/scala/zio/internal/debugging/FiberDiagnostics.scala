package zio.internal.debugging

import zio.{ Fiber, IO }
import zio.internal.{ SingleThreadedRingBuffer, Stack }
import zio.internal.stacktracer.ZTraceElement

case class FiberDiagnostics(
  fiberId: Fiber.Id,
  curZio: IO[Any, Any],
  stack: Stack[Any => IO[Any, Any]],
  stackTrace: SingleThreadedRingBuffer[ZTraceElement],
  execTrace: SingleThreadedRingBuffer[ZTraceElement],
  continuation: Runnable // run to unfreeze fiber
)
