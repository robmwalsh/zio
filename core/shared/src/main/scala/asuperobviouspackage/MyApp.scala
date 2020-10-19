package asuperobviouspackage

import zio.ZIO.break
import zio.console.putStrLn

object MyApp extends zio.App {

  def run(args: List[String]) =
    myAppLogic.exitCode

  val myAppLogic =
    for {
      _ <- putStrLn("Hello! What is your name?")
      _ <- putStrLn("Hello! What is your name?")
      _ <- putStrLn("Hello! What is your name?")
      _ <- break(true)
      _ <- putStrLn("Hello! What is your name?")
      _ <- putStrLn("Hello! What is your name?")
      _ <- putStrLn("Hello! What is your name?")
    } yield ()
}
