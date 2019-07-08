package server.utils

import akka.actor.{Actor, ActorRef}

class Task[A](parent: ActorRef) extends Actor {

  override def receive: Receive = {
    case StartTask(task) => task()
    case EndTask => context.system.terminate()
    case data =>
      parent ! data
      context.system.terminate()
    case _ => ()
  }
}
