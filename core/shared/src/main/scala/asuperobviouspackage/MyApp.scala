package asuperobviouspackage

import zio.UIO
import zio.UIO.succeed

object MyApp extends zio.App {

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic: UIO[Int] =
    succeed(1)
      .flatMap(x => succeed(x + 1))
}
