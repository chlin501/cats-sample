package sample

import cats._
import cats.data.{ State => CatsState }
import cats.implicits._

sealed trait Light {
  def next(): Light 
}
case object Green extends Light { 
  override def next(): Light = Yellow
}
case object Red extends Light {
  override def next(): Light = Green
}
case object Yellow extends Light {
  override def next(): Light = Red
}

sealed trait WaitTime {
  def waitFor(): Long
}
case object GreenWaitTime extends WaitTime {
  override def waitFor(): Long = 5000L
}
case object YellowWaitTime extends WaitTime {
  override def waitFor(): Long = 2000L
}
case object RedWaitTime extends WaitTime {
  override def waitFor(): Long = 4000L
}

object x {

  val state: CatsState[Light, Long] = CatsState(s => (s.next, { s.next match {
    case Green => GreenWaitTime.waitFor
    case Yellow => YellowWaitTime.waitFor
    case Red => RedWaitTime.waitFor
  }}))

  @throws(classOf[Exception])
  def main(args: Array[String]) {
    def read(color: Light): (Light, Long) = state.run(color).value
    def next(tuple: (Light, Long)) {
    }
    def f(color: Light) {
      val (next, waitTime) = state.run(color).value
      println(s"$next light, waiting for ${waitTime/1000.0} secs ... ")
      Thread.sleep(waitTime)
      f(next)
    }   
    f(Red)
  }
}


