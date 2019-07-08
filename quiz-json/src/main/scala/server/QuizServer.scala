package server

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp}
import server.user.{StartSession, UserSessionSupervisor}

case class SendMessage(data: String)

class QuizServer extends Actor{
  import context.system
  import Tcp._
  IO(Tcp) ! Bind(self, new InetSocketAddress("127.0.0.1", 9119))

  val sessionSupervisor: ActorRef = context.actorOf(Props(new UserSessionSupervisor()))

  def receive = {
    case bound @ Bound(localAddr) =>
      println("Server is bound to " + bound)
    case failed @ CommandFailed(_ : Bind) =>
      context stop self
    case connected @ Connected(remote, local) =>
      val connection = sender()
      println(remote.toString + "connected to " + local)
      sessionSupervisor ! StartSession(connection)
  }
}

object QuizServer {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("ActorSystem")
    val server = system.actorOf(Props(new QuizServer()))
  }
}