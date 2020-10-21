package asuperobviouspackage

import zio.ZIO.break
import zio.console.{ getStrLn, putStrLn }

object MyApp extends zio.App {

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic =
    for {
      _    <- break(true)
      _    <- putStrLn("Hello! What is your name?")
      _    <- break(true)
      name <- getStrLn
      _    <- putStrLn(s"Hello! $name")
    } yield ()
}
