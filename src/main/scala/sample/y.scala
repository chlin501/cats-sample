package sample

import cats._
import cats.data.{ State => CatsState }
import cats.implicits._

trait Event 
case object Start extends Event 
case object Restart extends Event 
case object ShutDown extends Event  
case object Recover extends Event 
trait Service extends Event 
case object RPCReady extends Service 
case object FileSystemReady extends Service 
case object BarrierSynchronizationReady extends Service
case object SchedulerReady extends Service
case object MonitorReady extends Service
trait ServiceFailure extends Event
case object RPCFailure extends ServiceFailure
case object FileSystemFailure extends ServiceFailure
case object BarrierSynchronizataionFailure extends ServiceFailure
case object SchedulerFailure extends ServiceFailure
case object MonitorFailure extends ServiceFailure
case object UnknownFailure extends Event

sealed trait MasterState {

  def next(): MasterState

}
/**
 * Possible State: Initializing
 */
case class Stopped(event: Option[Event] = None) extends MasterState {

  override def next(): MasterState = event.map { e => e match {
    case Start => Initializing()
    case _ => Failed(Seq(UnknownFailure))
  }}.getOrElse(Failed(Seq(UnknownFailure)))

}
/**
 * Possible States: 
 * - Running 
 * - Failed
 * - Initializing
 */
case class Initializing(events: Seq[Event] = Seq.empty[Event]) 
    extends MasterState {

  override def next(): MasterState = if(
    events.contains(RPCReady) &&
    events.contains(FileSystemReady) &&
    events.contains(BarrierSynchronizationReady) &&
    events.contains(SchedulerReady)
  ) Running() else if (
    !events.find(_.isInstanceOf[ServiceFailure]).isEmpty
  ) Failed(events) else Initializing(events)

}
/**
 * Possible States:
 * - ShuttingDown
 * - Failed
 */
case class Running(events: Seq[Event] = Seq.empty[Event]) extends MasterState {

  override def next(): MasterState = Running()

}
/**
 * Possible States:
 * - Stopped
 * - Failed
case object ShuttingDown extends MasterState {

}
 */
/**
 * Possible States:
 * - Running
 * - Failed
case object Recovering extends MasterState {

}
 */
/**
 * Possible States: 
 * - Recovering
 * - Stopped
 */
case class Failed(events: Seq[Event]) extends MasterState {

  override def next(): MasterState = Stopped()

}


object y {

/* TODO: bring in state with event so we can evaluate 
  val current: CatsState[MasterState, Unit] = CatsState(s => 
    s.copy(events = events) // TODO: find a way to pass events to here 
    (s, ())
  )
*/

  @throws(classOf[Exception])
  def main(args: Array[String]) {
    // def read(events: Event*) = current.run(events).value
  }
}

