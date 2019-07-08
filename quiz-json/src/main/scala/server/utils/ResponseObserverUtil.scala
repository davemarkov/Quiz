package server.utils

import akka.actor.ActorRef

import scala.collection.immutable.HashMap

case class StartTask(task: () => Unit)
case object EndTask

class ResponseObserverUtil[A]() {
  private var observerList: HashMap[ActorRef, A => Unit] = HashMap()

  def expect(task: ActorRef, handleFunction: A => Unit): Unit = {
    observerList += (task -> handleFunction)
  }

  def handleMessage(from: ActorRef, response: A): Unit = {
    observerList.get(from) match {
      case Some(completeAction) =>
        completeAction(response)
        // remove from list // should be dead
      case None => ()
    }
  }
}