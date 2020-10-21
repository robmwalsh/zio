package asuperobviouspackage

import zio.ZIO.break
import zio.console.{ getStrLn, putStrLn }

object MyApp extends zio.App {

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic =
    for {
      _        <- break(true)
      _        <- putStrLn("Hello! What is your name?")
      name     <- getStrLn
      _        <- putStrLn(s"Hello! $name, how are you?")
      response <- getStrLn
      _        <- putStrLn("that's good")
    } yield (response)
}
