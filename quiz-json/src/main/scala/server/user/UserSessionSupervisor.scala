package server.user

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp.{Register, Write}
import akka.util.ByteString
import server.quiz.DiscussionSupervisor
import server.storage.QuizDataBase

import scala.collection.immutable.List

case class StartSession(connection: ActorRef)
case class RemoveSession(child: ActorRef)

class UserSessionSupervisor extends Actor {
  private var connections: List[ActorRef] = List.empty
  val database: ActorRef = context.actorOf(Props(new QuizDataBase))
  val discussions: ActorRef = context.actorOf(Props(new DiscussionSupervisor(database)))

  def receive: Receive = {
    case StartSession(connection) => createUserSession(connection)
    case RemoveSession(child) => connections = connections.filter(elem => elem != child)
  }

  def createUserSession(connection: ActorRef): Unit = {
    val userSessionHandler = context.actorOf(Props(new UserSession(self, connection, discussions, database)))
    connection ! Register(userSessionHandler)
    connections = userSessionHandler :: connections
    sendMessage(connection, "Connected successfully. User session started.")
    userSessionHandler ! Login("someUser", "password")
  }

  def sendMessage(to: ActorRef, message: String): Unit = {
    to ! Write(ByteString(message))
  }

}
