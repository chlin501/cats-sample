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

sealed trait State {

  def next(): State

}
case class Stopped(event: Event) extends State {

  override def next(): State = event match {
    case Start => Initializing
    case _ => Failed
  }

}
case class Initializing(events: Events*) extends State {

  //override def possible(): Seq[State] = Seq(Failed, Running) 

  override def next(): State = if(
    events.contains(RPCReady) &&
    events.contains(FileSystemReady) &&
    events.contains(BarrierSynchronizationReady) &&
    events.contains(SchedulerReady)
  ) Running else if (events.isInstanceOf[Failed]) 
    Failed
  else Initializing(events)
}
case object Running extends State {

  //override def possible(): Seq[State] = Seq(ShuttingDown, Failed)

}
case object ShuttingDown extends State {

  //override def possible(): Seq[State] = Seq(Stopped, Failed)

}
case object Recovering extends State {

  //override def possible(): Seq[State] = Seq(Running, Failed)

}
case object Failed extends State {

  //override def possible(): Seq[State] = Seq(Recovering, Stopped)

}

object y {

/* TODO: bring in state with event so we can evaluate 
  val current: CatsState[State, Unit] = CatsState(s => 
    s.copy(events = events) // TODO: find a way to pass events to here 
    (s, ())
  )
*/

  @throws(classOf[Exception])
  def main(args: Array[String]) {
    // def read(events: Event*) = current.run(events).value
  }
}

