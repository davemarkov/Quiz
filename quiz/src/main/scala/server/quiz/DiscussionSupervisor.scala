package server.quiz

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.immutable.HashMap
import server.storage._

case class CreateQuiz(userName: String, quiz: Quiz)
case class GetQuiz(userName: String, quizName: String)
case class DeleteQuiz(userName: String, quizName: String)

abstract class DiscussionData
case class DiscussionAddress(quiz: ActorRef) extends DiscussionData

case class DiscussionSupervisorResponse(data: DiscussionData)

class DiscussionSupervisor(database: ActorRef) extends Actor {
  private var discussions: HashMap[String, ActorRef] = new HashMap[String, ActorRef]()

  def receive: Receive = {
    case CreateQuiz(userName, quiz) => createQuiz(userName, quiz, sender)
    case GetQuiz(userName, quizName) => returnQuiz(sender, quizName)
    case DeleteQuiz(userName, quizName) => deleteQuiz(userName, quizName)
  }

  def createQuiz(userName: String, quiz: Quiz, from: ActorRef): Unit = {
    val newDiscussion = context.actorOf(Props(new Discussion(quiz, userName, database)))
    database ! Create(quiz)
    database ! Create(DiscussionHistory(quiz.title, List()))
    discussions += (quiz.title -> newDiscussion)
    returnQuiz(from, quiz.title)
  }

  def deleteQuiz(userName: String, quizName: String): Unit = {

  }

  def respond(to: ActorRef, message: String): Unit = {
    to ! message
  }

  def returnQuiz(to: ActorRef, quizName: String): Unit = {
    discussions.get(quizName) match {
      case Some(value) => to ! DiscussionSupervisorResponse(DiscussionAddress(value))
      case None => // internal error
    }
  }

//  def initSavedDiscussions(): Unit = {
//    database !
//  }
}
