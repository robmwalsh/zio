package asuperobviouspackage

import zio.clock.Clock
import zio.internal.debugging.Debugger.BreakType

object LunchTime extends zio.App {

  import zio._
  import zio.console._
  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Attendee.
   */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def isStarving: STM[Nothing, Boolean] =
      state.get.map(_ == Starving)

    def feed: STM[Nothing, Unit] =
      state.set(Full)
  }
  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full     extends State
    }
  }

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Table.
   */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken)    => (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => true)

    def vacateSeat(index: Int): STM[Nothing, Unit] =
      seats.update(index, _ => false)

    def printState: STM[Nothing, Unit] =
      seats.foreach(seat => STM.succeed(println(seat)))
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds a single attendee.
   */
  def feedAttendee(t: Table, a: Attendee): URIO[Clock with Console, Unit] =
    for {
      seatIdx <- STM.atomically {
                   for {
                     seatIdx <- t.findEmptySeat.retryUntil(_.isDefined).map(_.get)
                     //seatIdx <- t.findEmptySeat.repeatUntil(_.isDefined).map(_.get)
                     _ <- t.takeSeat(seatIdx)
                   } yield seatIdx
                 }
      _ <- putStrLn(s"$a took seat $seatIdx")
      _ <- a.feed.commit
      //_ <- sleep(1.millis)
      _ <- putStrLn(s"$a has been fed at $seatIdx")
      _ <- t.vacateSeat(seatIdx).commit
      _ <- putStrLn(s"$a left seat $seatIdx")
    } yield ()

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds only the starving attendees.
   */
  def feedStarving(table: Table, list: List[Attendee]): RIO[Clock with Console, Unit] =
    ZIO.foreachPar_(list)(attendee => feedAttendee(table, attendee))

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val Attendees = 100
    val TableSize = 5
    ZIO.break(BreakType.All) *>
      ZIO
        .foreach(List.fill(Attendees)(Attendee.State.Starving))(starving =>
          TRef
            .make[Attendee.State](starving)
            .map(Attendee(_))
            .commit
        )
        .flatMap(attendees =>
          TArray
            .fromIterable(List.fill(TableSize)(false))
            .map(Table)
            .commit
            .flatMap(table =>
              feedStarving(table, attendees).orDie
                .map(_ => ExitCode.success)
            )
        )
  }
}
