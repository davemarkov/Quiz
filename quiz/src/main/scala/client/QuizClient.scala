package client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp}
import akka.util.ByteString


object Client {
  def props(remote: InetSocketAddress) =
    Props(classOf[Client], remote)
}

class Client(remote: InetSocketAddress, listener: ActorRef) extends Actor {

  import akka.io.Tcp._
  import context.system

  IO(Tcp) ! Connect(remote)

  def receive = {
    case CommandFailed(_: Connect) =>
      println("failed")
      context stop self

    case c @ Connected(remote, local) =>
      println(c)
      val connection = sender()
      connection ! Register(self)

      context become {
        case data: ByteString => connection ! Write(data)
        case CommandFailed(w: Write) => // O/S buffer was full
        case Received(data) => listener ! data
        case "close" => connection ! Close
        case _: ConnectionClosed => context stop self
      }
      connection ! Write(ByteString("connection established on client"))
  }
}

class ClientInterface extends Actor {
  override def receive: Receive = {
    case data: ByteString =>
      println(data.decodeString("US-ASCII"))
  }
}


object QuizClient {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("ActorSystem")
    val listener = system.actorOf(Props(new ClientInterface()))
    val client = system.actorOf(Props(new Client(new InetSocketAddress("127.0.0.1", 9119), listener)))
    Thread.sleep(5000)
    client ! ByteString("it is done")
  }
}



