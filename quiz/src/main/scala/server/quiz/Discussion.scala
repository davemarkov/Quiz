package server.quiz

import akka.actor.{Actor, ActorRef, Props}
import server.storage._
import server.utils.{ResponseObserverUtil, StartTask, Task}

case class Comment(username:String, timestamp: String, text: String)
case class DiscussionHistory(quizName: String, history: List[Comment]) extends StorageData

case class History(history: List[Comment])

import scala.collection.immutable.HashMap

case class Subscribe(username: String)
case class Unsubscribe(username: String)
case class RequestHistory(username: String)
case class SendComment(comment: Comment)

class Discussion(quiz: Quiz, owner: String, database: ActorRef) extends Actor {
  private var subscribedUsers: HashMap[String, ActorRef] = HashMap()
  private val responseObserver: ResponseObserverUtil[DataBaseResponse] = new ResponseObserverUtil()

  def receive: Receive = {
    case Subscribe(username: String) => subscribeUser(username, sender)
    case Unsubscribe(username: String) => unsubscribeUser(username, sender)
    case RequestHistory(username: String) => returnHistory(username, sender)
    case comment@Comment(_,_,_) => updateDiscussion(comment, sender)
  }

  def broadcast(toExclude: ActorRef, comment: Comment): Unit = {
    for ((_, userRef) <- subscribedUsers if userRef != toExclude) userRef ! comment
  }

  def send(user: ActorRef, message: String): Unit = {
      user ! message
  }

  def subscribeUser(username: String, userRef: ActorRef): Unit = {
    subscribedUsers += (username -> userRef)
  }

  def unsubscribeUser(name: String, user: ActorRef): Unit = {
    subscribedUsers -= name
  }

  def returnHistory(username: String, user: ActorRef): Unit = {
    val newTask: ActorRef =
      context.system.actorOf(Props(new Task(self)))

    responseObserver.expect(newTask, (response: DataBaseResponse) => {
      response.data match {
        case List(DiscussionHistory(_, history)) => user ! History(history)
        case _ => () // send error or something
      }
    })
    newTask ! StartTask(() => {
      database ! Select(DiscussionHistory(quiz.title, List()), Has)
    })
  }

  def updateDiscussion(comment: Comment, user: ActorRef): Unit = {
    database ! Update(DiscussionHistory(quiz.title, List(comment)))
    broadcast(user, comment)
  }
}
